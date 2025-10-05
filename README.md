# Gemini Chat Bot for Emacs

```bash
echo "<YOUR-API-KEY>" > .api-key.txt
```

## Doom Emacs Config

### packages.el

```lisp
(package! gemini
  :recipe (:host github :repo "zeroflag/gemini.emacs"))
```

### config.el

```lisp
(use-package! gemini
  :commands gemini-interactive-send
  :defer t
  :init
  (setq gemini-api-key-file "~/path/to/.api-key.txt")
  (map! :leader
        :desc "Send buffer to Gemini Assistant"
        "g g" #'gemini-interactive-send))
```
