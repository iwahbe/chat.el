# chat.el

![Chat Demo](./gifs/chat-demo.gif)

`chat.el` is an pure Emacs package for interacting with OpenAI's ChatGPT models such as GPT3. It interacts directly with the OpenAI API vie Emac's built-in `url` package.

It offers several helpful features that enable users to perform, such as writing this README for me. Functionality can be broken down into two basic categories.

### Transient Queries
The package offers different ways of querying the ChatGPT model based on user input and region selection, which include:

- `chat-query-user`: Query ChatGPT's response to a user-specified input, either displaying or inserting it.
- `chat-query-region`: Sends the active region to ChatGPT for responses relevant to the content.
- `chat-query-dwim`: A prompt for querying ChatGPT prompts users to either specify a region of text or ask for machine learning recommendations based on a prompt.

### Interactive ChatGPT conversation
A `chat-mode` allows users to communicate with the ChatGPT model in a "conversation-like" format with prompts. To start an interactive session, run `chat`.

The package also supports `custom.el`, facilitating configuration of API keys and user-specific settings.

## Configuration

In the configuration section, `chat-api-env-key` set the environmental variable used to look up an API key if not specified, while `chat-api-key` sets the OpenAI API key directly.

- `chat-max-tokens` sets the maximum number of tokens for ChatGPT to return for a single request.

- `chat-system-prompt` sets the system prompt given to all ChatGPT requests.

- `chat-user-prompt` sets the text prompt for by the user when in `chat-mode`. Defaults to `"You > "`.

- `chat-bot-prompt` sets the prompt faced by the AI in `chat-mode`. Defaults to `"Bot > "`.

- `chat-tempature` sets the `tempature` parameter for requests. Defaults to `nil`.

- `chat-top-p` sets the `top_p` parameter for requests. Defaults to `nil`.

## Installation

To install `chat.el`, follow these simple steps:

1. Download "chat.el" file to your machine.

2. In your Emacs configuration file, add this line:

```emacs-lisp
         (add-to-list 'load-path "/path/to/chat.el/directory")
```

3. Evaluating `(require 'chat)` in emacs will load the package.

4. If you want to give a custom configuration, add your configuration to your emacs configuration file.

To configure this package, customize `chat-mode` to control the Chat GPT interaction behavior.
