;;; chat.el --- An Emacs facade for ChatGPT  -*- lexical-binding:t -*-

(require 'json)
(require 'url)
(require 'url-http)

(defgroup chat ()
  "Chatting with OpenAI models, such as GPT3."
  :group 'external)

(defcustom chat-api-env-key "OPENAI_API_KEY"
  "The environmental variable used to look up an API key if not specified."
  :type 'string :group 'chat)

(defcustom chat-api-key nil
  "The API Key used to query api.openapi.com."
  :type 'string :group 'chat)

(defcustom chat-max-tokens 500
  "The maximum number of tokens for ChatGPT to return for a single request.
A nil value translates to no upper bound in the number of tokens."
  :type 'number :group 'chat)

(defcustom chat-system-prompt "You are a helpful AI Assistant embedded into Emacs."
  "The system prompt given to all ChatGPT requests."
  :type 'string :group 'chat)

(defvar chat--input-history nil
  "The variable that stores input history.")

(defun chat-get-api-key ()
  "Get API key for OpenAI.
If no key is found, error."
  (or chat-api-key
      (and chat-api-env-key (getenv chat-api-env-key))
      (user-error
       "Could not get OpenAI API key. Either set `chat-api-key' or ensure that \"%s\" is set"
       chat-api-env-key)))

(defun chat--async-raw (messages callback finalize)
  "Query ChatGPT with MESSAGES.
CALLBACK is called in the receiving buffer with POINT at the
beginning of the new data.
FINALIZE is called after all data has been processed."
  (let* ((url-request-method "POST")
         (url-request-extra-headers `(("Content-Type" . "application/json")
                                      ("Authorization" . ,(concat "Bearer " (chat-get-api-key)))))
         (url-request-data (json-encode
                            `(("model" . "gpt-3.5-turbo")
                              ,@(when chat-max-tokens
                                  `(("max_tokens" . ,chat-max-tokens)))
                              ("stream" . t)
                              ("messages" . ,messages))))
         (filter (lambda (filter proc data)
                   (let ((length (funcall filter proc data))
                         (b (process-buffer proc)))
                     (when (buffer-live-p b)
                       (with-current-buffer b
                         (goto-char (- (point-max) (length data)))
                         (funcall callback))
                       length)))))
    (advice-add #'url-http-generic-filter :around filter)
    (url-retrieve "https://api.openai.com/v1/chat/completions"
                  (lambda (status)
                    (advice-remove #'url-http-generic-filter filter)
                    (when-let (err (plist-get status :error))
                      (error "Failed to get response: %s" err))
                    (when finalize
                      (funcall finalize))))))

(defun chat--async-query (messages callback finalize)
  "Apply CALLBACK to each data block returned from calling ChatGPT on MESSAGES.
FINALIZE is called when the query finishes."
  (let ((b (current-buffer)))
    (chat--async-raw
     (cons `(("role" . "system")
             ("content" . ,chat-system-prompt))
           messages)
     (lambda ()
       (while (and (search-forward "data:" nil t)
                   (not (string= " [DONE]"
                                 (buffer-substring-no-properties
                                  (point)
                                  (min (point-max) (+ 7 (point)))))))
         (let ((chunk (json-parse-buffer
                       :array-type 'list
                       :object-type 'alist)))
           (with-current-buffer b
             (funcall callback chunk)))))
     finalize)))

(defun chat--async-text (input callback &optional finalize)
  "Make a call to ChatGPT on INPUT and call CALLBACK on the resulting text chunks.
FINALIZE is called when the text is over."
  (chat--async-query
   input
   (lambda (x)
     (funcall callback
              (alist-get 'content
                         (alist-get 'delta
                                    (car (alist-get 'choices x))))))
   finalize))

(defun chat-query-user (input &optional insert)
  "Insert ChatGPTs response to INPUT.
If INSERT is non-nil, the text is inserted into the current buffer."
  (interactive "sChatGPT Input: \nP")
  (if insert
      ;; If we are inserting, we need to get a reference to where the point is now.
      (let ((p (point-marker)))
        (barf-if-buffer-read-only)
        (chat--async-text
         `((("role" . "user") ("content" . ,input)))
         (lambda (chunk)
           (when chunk
             (with-current-buffer (marker-buffer p)
               (goto-char (marker-position p))
               (insert-before-markers-and-inherit chunk))))))
    ;; Otherwise, create a new buffer to display the output
    (let ((b (get-buffer-create "ChatGPT Query")))
      (pop-to-buffer b)
      (with-current-buffer b
        (erase-buffer)
        (special-mode)
        (chat--async-text
         `((("role" . "user") ("content" . ,input)))
         (lambda (chunk)
           (when chunk
             (let ((inhibit-read-only t)) (insert chunk)))))))))

(defmacro chat--with-query-buffer (&rest body)
  "Run BODY in a temporary query buffer."
  (let ((s (gensym)))
    `(let ((,s (get-buffer-create "ChatGPT Query")))
       (pop-to-buffer ,s)
       (with-current-buffer ,s
         (special-mode)
         (let ((inhibit-read-only t)) (erase-buffer))
         ,@body))))

(defun chat-query-region (reg-beg reg-end &optional mode)
  "Apply INPUT to the region bounded by REG-BEG and REG-END.
MODE determines what is done with the result.
- If nil, a new buffer is created to hold the output."
  (interactive "r\nP")
  (when mode
    (barf-if-buffer-read-only))

  ;; TODO: Right now, this only works in the commentary mode. We need to add the "insert"
  ;; and "replace" modes.

  (let* ((input (read-string "ChatGPT Input (applied to region): "))
         (contents (buffer-substring-no-properties
                    reg-beg reg-end)))
    (chat--with-query-buffer
     (special-mode)
     (chat--async-text
      `((("role" . "user") ("content" . "Apply the next input as context going forward"))
        (("role" . "user") ("content" . ,contents))
        (("role" . "user") ("content" . ,input)))
      (lambda (chunk)
        (when chunk
          (let ((inhibit-read-only t)) (insert chunk))))))))

(defun chat-query-dwim (&optional arg)
  "Query ChatGPT, getting input via a region or with the prompt.

This is not designed for programmatic use."
  (if (region-active-p)
      (chat-query-region (region-beginning)
                         (region-end)
                         arg)
    (funcall-interactively (chat-query-user arg))))


;;; Interactive ChatGPT conversation: `chat-mode'.


(defvar-local chat--entries nil
  "The location and contents of local entries in a `chat-mode' buffer.")

(defcustom chat-user-prompt "You > "
  "The prompt facing the user when in `chat-mode'."
  :type 'string :group 'chat)

(defcustom chat-bot-prompt "Bot > "
  "The prompt facing the bot when in `chat-mode'."
  :type 'string :group 'chat)

(defface chat-prompt '((t . (:inherit font-lock-keyword-face)))
  "The face used for prompts."
  :group 'chat)

(defface chat-bot '((t . (:inherit font-lock-string-face)))
  "The face used for ChatGPT's responses.")

(defvar chat-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'chat-send)
    (define-key m (kbd "C-RET") #'newline)
    (define-key m (kbd "<return>") #'chat-send)
    (define-key m (kbd "C-<return>") #'newline)
    m)
  "The mode map for `chat-mode'.")

(defun chat ()
  "Enter an interactive session with ChatGPT."
  (interactive)
  (let ((b (get-buffer-create "ChatGPT")))
    (pop-to-buffer b)
    (with-current-buffer b
      (chat-mode)
      (chat--insert-prompt chat-user-prompt))))

(defun chat--insert-prompt (prompt)
  "Insert a new PROMPT, ending the previous entry.
PROMPT must be a recognized `chat-mode' prompt."
  (insert (propertize prompt
                      'face 'chat-prompt
                      'read-only t
                      'rear-nonsticky t))
  (setq chat--entries
        (cons
         (list (cond
                ((string= prompt chat-user-prompt) 'user)
                ((string= prompt chat-bot-prompt) 'bot)
                (t (error "Unknown prompt: %s" prompt)))
               (point))
         chat--entries)))

(defun chat--finish-entry ()
  "Finish the current entry."
  (setf (car chat--entries)
        `(,@(car chat--entries)
          ,(buffer-substring-no-properties (cadar chat--entries) (point-max))
          ,(point-max))))

(defun chat-send ()
  "Send the next command to ChatGPT.
This works only in a `chat-mode' buffer."
  (interactive)
  (unless (derived-mode-p 'chat-mode)
    (error "`chat-send' should only be called in `chat-mode'"))
  (unless chat--entries
    (error "`chat-mode' is not properly set up, missing `chat--entries'"))
  (let* ((prompt-end (car (last (car-safe chat--entries))))
         (entry (buffer-substring-no-properties prompt-end (point-max)))
         (inhibit-read-only t))
    (if (string-empty-p (string-trim entry))
        (progn
          (goto-char (point-max))
          (newline)
          (add-text-properties (point-min) (point-max)
                               '(read-only t rear-nonsticky nil))
          ;; Mark the whole buffer as read-only. We want to prevent the user from altering
          ;; our saved positions.
          (chat--insert-prompt chat-user-prompt))
      ;; Finalize the entry
      (chat--finish-entry)
      ;; Now insert the AI prompt
      (newline)
      (chat--insert-prompt chat-bot-prompt)
      ;; Lock the buffer, ensuring that only we can edit it.
      (add-text-properties (point-min) (point-max)
                           '(read-only t rear-nonsticky nil))
      (chat--send-entries
       (lambda ()
         (let ((inhibit-read-only t))
           (newline)
           (chat--insert-prompt chat-user-prompt)))))))

(defun chat--send-entries (finish)
  "Send the current buffers entries as a single request to CHATGPTs.
FINISH is called after all text has been inserted."
  (let ((b (current-buffer)))
    (chat--async-text
     (mapcar (lambda (entry)
               `(("role" . ,(pcase (car entry)
                              ('user "user")
                              ('bot "assistant")))
                 ("content" . ,(caddr entry))))
             (reverse (cdr chat--entries)))
     (lambda (chunk)
       (when chunk
         (with-current-buffer b
           (goto-char (point-max))
           (let ((inhibit-read-only t))
             (insert
              (propertize chunk
                          'read-only t
                          'rear-nonsticky t
                          'face 'chat-bot))))))
     (lambda ()
       (with-current-buffer b
         (chat--finish-entry)
         (funcall finish))))))

(define-derived-mode chat-mode nil "Chat"
  "The major mode used for extended conversations with ChatGPT."
  :group 'chat)

(provide 'chat)
;;; chat.el ends here
