# To Fix ECB

Not yet sure whether these steps work or are even worth it.

## Prereqs

* Go ahead and install 'ecb' from ELPA
* Be using emacs 24.3+

## Fix Steps

The **ecb** files will be located under the ELPA directory structure.

1) Find `(setq version-error nil)` in **ecb-uprade.el**
    * Find the if-statement that performs the version check
    * Short-circuit the check to continue

2) Correct the following in **ecb.el**, change:

    (ecb-enable-own-temp-buffer-show-futition)
    
   to 
    
    (ecb-enable-own-temp-buffer-show-function)

3) Delete **ecb.elc** and **ecb-upgrade.elc**

4) M-x `byte-compile-file` **ecb.el**  and **ecb-upgrade.el**

5) Add `(setq stack-trace-on-error t)` in **~/.emacs** or **~/.emacs.d/init.el**
