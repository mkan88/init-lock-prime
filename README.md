Prevents you from meddling with the init file. To use, set a list of files to lock,
then enable locking in your config file:

```
(customize-set-variable 'init-lock-files '("/home/mkan/.spacemacs"))
(init-lock-enable)
```
