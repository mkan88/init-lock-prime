Prevents you from meddling with the init file. To use, set a list of files to lock,
and enable locking in your config file:

```
(custom-set-variables '(init-lock-files '("/home/user/.spacemacs")))
(init-lock-enable)
```
