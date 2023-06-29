# GPT-Commit: Generate Conventional Commit Messages with GPT in Emacs

![Screenshot](screenshots/magit-commit.png)

GPT-Commit is an Emacs package that automatically generates conventional commit messages using the [GPT (Generative Pre-trained Transformer)](https://openai.com/research/gpt-3) model from OpenAI.

## Table of Contents

- [Installation](#installation)
- [Configuration](#configuration)
- [Usage](#usage)
- [License](#license)

## Installation

### Via MELPA

If you have MELPA configured, you can easily install `gpt-commit` from within Emacs by running:

```emacs-lisp
M-x package-install RET gpt-commit RET
```

### Manual

To install this package manually, clone this repository and add the following to your `.emacs` or `init.el`:

```emacs-lisp
(add-to-list 'load-path "/path/to/gpt-commit")
(require 'gpt-commit)
```

## Configuration

1. **Set OpenAI API Key**: You will need an API key from OpenAI to use the GPT model. Set the key like this:

```emacs-lisp
(setq gpt-commit-openai-key "YOUR_OPENAI_API_KEY")
```

2. **Set GPT Model Name** (Optional): By default, it uses the `gpt-3.5-turbo` model. If you wish to use a different model, you can set it like this:

```emacs-lisp
(setq gpt-commit-model-name "YOUR_PREFERRED_MODEL_NAME")
```

3. **Add Hook**: Add the `gpt-commit-message` function to the `git-commit-setup-hook` to automatically generate commit messages when the commit message editor starts:

```emacs-lisp
(require 'gpt-commit)
(add-hook 'git-commit-setup-hook 'gpt-commit-message)
```

## Usage

Once you have configured `gpt-commit`, it will automatically generate a commit message using the GPT model whenever you perform a Git commit. You can accept the message by saving and closing the editor, or you can modify the message as needed.

Please note that using the OpenAI API might incur costs, and there might be usage limitations. Be aware of the [OpenAI pricing and policy](https://openai.com/pricing) before extensive use.

## License

GPT-Commit is licensed under the [GNU General Public License v3.0](LICENSE).
