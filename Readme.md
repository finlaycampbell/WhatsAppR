[![Build Status](https://travis-ci.org/finlaycampbell/WhatsAppR.svg?branch=master)](https://travis-ci.org/finlaycampbell/WhatsAppR) [![codecov](https://codecov.io/gh/finlaycampbell/WhatsAppR/branch/master/graph/badge.svg)](https://codecov.io/gh/finlaycampbell/WhatsAppR)

```html
<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.
```


# WhatsAppR: Visualising WhatsApp Conversation Data

This package visualises various features of your WhatsApp conversations with different people and groups; what time of day you message most, what words you use most commonly, who messages more and who replies more quickly.


## Installing the package

To install the devel version of the package, type:
  
  ```r
  devtools::install_github("finlaycampbell/WhatsAppR")
  ```

Note that this requires the package *devtools* installed.


## Importing WhatsApp conversation data

To import a conversation of interest into WhatsAppR:

-select the conversation on the mobile WhatsApp app <br />
-select additional options -> 'More' -> 'Email chat' <br />
-email the conversation to yourself <br />
-download the .txt file and provide the file path as an argument to WhatsAppR  


### Maintainer:
Finlay Campbell (@finlaycampbell // f.campbell15@imperial.ac.uk)
