;;; chat.el --- An Emacs facade for ChatGPT  -*- lexical-binding:t -*-

;; Copyright (C) 2023 Ian Wahbe

;; Author: Ian Wahbe
;; URL: https://github.com/iwahbe/chat.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This is a package `chat.el` for asynchronous interaction with OpenAI's ChatGPT models,
;; such as GPT3, in Emacs.

;; The package provides several features that include:

;; ### Transient interactions based on user input and the region.
;; - `chat-query-user`: Insert ChatGPT's response to a user-specified input.
;; - `chat-query-region`: Query ChatGPT and replace/insert ChatGPT's response as indicated by given parameters
;; - `chat-query-dwim`: A prompt for querying ChatGPT, determines whether the user would like to query by region or prompt.

;; ### Interactive ChatGPT conversation: `chat-mode`.
;; - `chat-send`: Allows chat with the AI in a "conversation-like" way

;; ### Public functions
;; - `chat-send`: An interactive function for sending input to ChatGPT
;; - `chat-query-region`: Send a selected region to ChatGPT for responses
;; - `chat-query-user`: Send user-input to ChatGPT for responses
;; - `chat`: An interactive session for chatting with a ChatGPT agent.

;; This package also has `custom.el` support for configuring API keys and other user-specific settings.

;;; Code:

(require 'json)
(require 'url)
(require 'url-http)
(require 'cl-lib)

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

(defcustom chat-temperature nil
  "Set the temperature value sent to ChatGPT.
A nil value results in the default value being used.
The OpenAI reference docs for temperature say:

  What sampling temperature to use, between 0 and 2. Higher values
  like 0.8 will make the output more random, while lower values
  like 0.2 will make it more focused and deterministic.

  We generally recommend altering this or top_p but not both."
  :type 'float :group 'chat)

(defcustom chat-top-p nil
  "Set the top_p value sent to ChatGPT.
A nil value results in the default value being used.
The OpenAI reference docs for top_p say:

  An alternative to sampling with temperature, called nucleus
  sampling, where the model considers the results of the tokens
  with top_p probability mass.  So 0.1 means only the tokens
  comprising the top 10% probability mass are considered.

  We generally recommend altering this or temperature but not
  both."
  :type 'float :group 'chat)

(defcustom chat-model "gpt-3.5-turbo"
  "The chat model used during conversations.

The API token provided must have access to the model used."
  :type '(choice
    (const "gpt-4")
    (const "gpt-4-0314")
    (const "gpt-4-32k")
    (const "gpt-4-32k-0314")
    (const "gpt-3.5-turbo")
    (const "gpt-3.5-turbo-0301"))
  :group 'chat)

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

(defvar chat--input-history nil
  "The variable that stores input history.")

(defun chat-get-api-key ()
  "Get API key for OpenAI.
If no key is found, error."
  (or chat-api-key
      (and chat-api-env-key (getenv chat-api-env-key))
      (user-error
       "Could not get OpenAI API key.  Either set `chat-api-key' or ensure that \"%s\" is set"
       chat-api-env-key)))

;;; Asynchronous primitives for streaming data from ChatGPT's API


(defun chat--async-raw (messages callback finalize)
  "Query ChatGPT with MESSAGES.
CALLBACK is called in the receiving buffer with POINT at the
beginning of the new data.
FINALIZE is called after all data has been processed."
  (let* ((url-request-method "POST")
         (url-request-extra-headers `(("Content-Type" . "application/json")
                                      ("Authorization" . ,(concat "Bearer " (chat-get-api-key)))))
         (url-request-data (encode-coding-string
                            (json-encode
                             `(("model" . ,chat-model)
                               ,@(when chat-max-tokens
                                   `(("max_tokens" . ,chat-max-tokens)))
                               ,@(when chat-temperature
                                   `(("temperature" . ,chat-temperature)))
                               ,@(when chat-top-p
                                   `(("top_p" . ,chat-top-p)))
                               ("stream" . t)
                               ("messages" . ,messages)))
                            'utf-8))
         (filter (lambda (filter proc data)
                   (let ((length (funcall filter proc data))
                         (b (process-buffer proc)))
                     (when (buffer-live-p b)
                       (with-current-buffer b
                         (goto-char (- (point-max) (length data)))
                         (funcall callback))
                       length)))))
    (advice-add #'url-http-generic-filter :around filter)
    (unless (url-retrieve "https://api.openai.com/v1/chat/completions"
                          (lambda (status)
                            (advice-remove #'url-http-generic-filter filter)
                            (when finalize
                              (funcall finalize))
                            (when-let (err (plist-get status :error))
                              ;; We got an error. Lets try to parse the buffer and surface
                              ;; a good error response
                              (error "%s: %s" (cdr err)
                                     (condition-case nil
                                         (progn
                                           (goto-char (point-min))
                                           (search-forward "\n\n")
                                           (let* ((parsed (json-parse-buffer
                                                           :array-type 'list
                                                           :object-type 'alist))
                                                  (e (alist-get 'error parsed)))
                                             (or (alist-get 'message e)
                                                 (error "Could not get message"))))
                                       ;; Something went wrong getting the error message.
                                       ;; We will just show the whole buffer in the error
                                       ;; message.
                                       (error (buffer-substring (point-min) (point-max))))))
                            (kill-buffer)))
      ;; From the documentation of `url-retrieve':
      ;;
      ;;   Return the buffer URL will load into, or nil if the process has already
      ;;   completed (i.e. URL was a mailto URL or similar; in this case the callback is
      ;;   not called).
      ;;
      ;; We need to make sure that `finalize' is always called.
      (advice-remove #'url-http-generic-filter filter)
      (when finalize
        (funcall finalize)))))

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

;;; Transient interactions based on user input and the region.


(defmacro chat--with-query-buffer (input insert &rest body)
  "Run BODY with access temporary query buffer to display the result.
INSERT is bound to a function that will insert text into the buffer.
INPUT is displayed to the user as the model's input."
  (declare (indent defun))
  (let ((s (gensym)))
    `(let ((,s (get-buffer-create "ChatGPT Query")))
       (pop-to-buffer ,s)
       (with-current-buffer ,s
         (chat-query-mode)
         (let ((inhibit-read-only t))
           (erase-buffer)
           (chat--query-mode-insert-heading
            (setq chat--query-input ,input))))
       (cl-flet ((,insert (text) "Insert text into the query buffer"
                   (with-current-buffer ,s
                     (goto-char (point-max))
                     (let ((inhibit-read-only t))
                       (insert text)))))
         ,@body))))

(defvar-local chat--query-input nil
  "The input to the current query.")

(defun chat--query-mode-insert-heading (input)
  "Insert the standard heading for `chat-query-mode'.
Display INPUT as the model's Input."

(insert (substitute-command-keys
           (propertize
            "To reply, press \\[chat-query-reply].\n\n"
            'face 'shadow)))
  (insert input "\n\n")
  (insert (propertize "---" 'face 'shadow) "\n\n"))

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
    (chat--with-query-buffer input insert
      (chat--async-text
       `((("role" . "user") ("content" . ,input)))
       (lambda (chunk)
         (when chunk (insert chunk)))))))

(defun chat-query-region (reg-beg reg-end &optional mode)
  "Apply INPUT to the region bounded by REG-BEG and REG-END.
MODE determines what is done with the result.

- If nil, a new buffer is created to hold the output.

- If mode is 4 (\\[universal-argument]), then `chat-query-region'
  inserts its contents after point.
- If mode is 16 (\\[universal-argument] \\[universal-argument]), then `chat-query-region'
  replaces the region with ChatGPT's response."
  (interactive "r\nP")
  (when mode
    (barf-if-buffer-read-only))
  (let* ((input (read-string "ChatGPT Input (applied to region): "))
         (contents (buffer-substring-no-properties
                    reg-beg reg-end))
         (messages `((("role" . "user") ("content" . ,(concat "I'm going to give an instruction, \
then the object to run the instruction on. The command is: " input)))
                     (("role" . "assistant") ("content" . "What text should I perform the instruction on?"))
                     (("role" . "user") ("content" . ,contents)))))
    (pcase (car-safe mode)
      ;; Inserting text into a query buffer.
      ((pred not)
       (chat--with-query-buffer input insert
         (special-mode)
         (chat--async-text
          messages
          (lambda (chunk) (when chunk (insert chunk))))))
      ;; Inserting text at point.
      (4 (let ((p (point-marker)))
           (chat--async-text
            messages
            (lambda (chunk)
              (when chunk
                (with-current-buffer (marker-buffer p)
                  (goto-char (marker-position p))
                  (insert-before-markers-and-inherit chunk)))))))
      ;; Replacing the region with the AI's output.
      (16 (let ((beg (save-excursion (goto-char reg-beg) (point-marker)))
                (end (save-excursion (goto-char reg-end) (point-marker))))
            ;; Here, we mark the text to be replaced as such, preventing the user from
            ;; making pointless edits.
            (add-text-properties reg-beg reg-end
                                 '(read-only t face shadow))
            (chat--async-text
             messages
             (lambda (chunk)
               "Insert CHUNK into the buffer, gradually replacing the previous text."
               (when chunk
                 (with-current-buffer (marker-buffer beg)
                   (let* ((region-insert-end (min (marker-position end)
                                                  (+ (length chunk)
                                                     (marker-position beg))))
                          (region-insert-length (- region-insert-end
                                                   (marker-position beg))))
                     (when (>= region-insert-length 0)
                       ;; When we have room to insert into the current region
                       (let ((inhibit-read-only t))
                         ;; Delete the old content, go to the beginning and insert Since
                         ;; we delete content after `beg' and insert the new content
                         ;; before `beg', this has the effect of inching `beg' towards
                         ;; end.
                         (delete-region beg region-insert-end)
                         (goto-char beg)
                         (insert-before-markers-and-inherit (substring chunk 0 region-insert-length))))
                     (goto-char beg)
                     ;; Any remaining content is inserted at the beginning. If we have not
                     ;; yet overwritten the full string, this will be a no-op.
                     (insert-before-markers-and-inherit (substring chunk region-insert-length))))))
             (lambda ()
               "Remove any remaining part of the region to be replaced."
               (with-current-buffer (marker-buffer beg)
                 (let ((inhibit-read-only t))
                   (delete-region  beg end)))))))
      (_ (user-error "Unknown mode: %s" mode)))))

(defun chat-query-dwim (&optional arg)
  "Query ChatGPT, getting input via a region or with the prompt.

This is not designed for programmatic use.  ARG is passed as the
mode controller to `chat-query-user' and `chat-query-region'."
  (if (region-active-p)
      (chat-query-region (region-beginning)
                         (region-end)
                         arg)
    (funcall-interactively (chat-query-user arg))))

(defvar chat-query-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "r") #'chat-query-reply)
    m)
  "The key map for `chat-query-mode'.")

(defun chat-query-reply ()
  "Reply the currently displayed query."
  (interactive)
  (let ((input chat--query-input)
        (response (progn
                    (goto-char (point-min))
                    (search-forward (concat chat--query-input "\n\n---\n\n"))
                    (buffer-substring-no-properties (point) (point-max)))))
    (chat-mode)
    (read-only-mode -1)
    (erase-buffer)
    ;; Now that we have entered chat mode, we fast forward the conversation to catch up.
    ;; We first insert the user's original prompt:
    (chat--insert-prompt chat-user-prompt)
    (insert input)
    (chat--finish-entry)
    (newline)
    ;; Then we insert the chat-bot's response.
    (chat--insert-prompt chat-bot-prompt)
    (insert (propertize response 'face 'chat-bot))
    (chat--finish-entry)
    (newline)
    (let ((p (point)))
      (chat--insert-prompt chat-user-prompt)
      (add-text-properties (point-min) p
                           '(read-only t rear-nonsticky nil)))))


(define-derived-mode chat-query-mode special-mode "Chat"
  "The major mode for viewing a ChatGPT query."
  :group 'chat
  :interactive nil)

;;; Interactive ChatGPT conversation: `chat-mode'.


(defvar-local chat--entries nil
  "The location and contents of local entries in a `chat-mode' buffer.")

(defvar chat-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'chat-send)
    (define-key m (kbd "C-RET") #'newline)
    (define-key m (kbd "<return>") #'chat-send)
    (define-key m (kbd "C-<return>") #'newline)
    m)
  "The mode map for `chat-mode'.")

(defun chat (&optional arg)
  "Enter an interactive session with ChatGPT.
If ARG is non-nil, switch to a new buffer instead of popping to a new buffer."
  (interactive "P")
  (let ((b (get-buffer-create "ChatGPT")))
    (if arg
        (switch-to-buffer b)
      (pop-to-buffer b))
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
  (when (eq 'bot (caar chat--entries))
    (user-error "Waiting on the bot to finish speaking."))
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
  :group 'chat
  :interactive nil)

(provide 'chat)
;;; chat.el ends here
