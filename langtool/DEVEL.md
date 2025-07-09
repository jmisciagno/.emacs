# Java

## Debian(stable)

Build new JDK to try the newest LanguageTool.
https://wiki.debian.org/JavaPackage

# Test

## make check

Currently almost test is empty although.

## Manual tests

### Check is working

Try to check some buffer. (C-x 4 w)

  1. Command line
  2. Server <-> Client
  3. Client
    manually invoke following in shell:
    java -cp *langtool-language-tool-server-jar* org.languagetool.server.HTTPServer -p 8082
    
    set following:
    (setq langtool-http-server-host "localhost"
          langtool-http-server-port 8082)

### Interactive correction is working

Try to correct buffer (C-x 4 c)


# TODO

- more unit test ( for each 4.x )
- check version dependent code. (old code is remaining)
- automated build/test on Github Action (27.x, 28.x, latest)
- langtool-popup.el: langtool-details-error-message -> langtool-simple-error-message as customize option

# Release

1. Test (`make check` and manual test)
2. Add tag (`git tag **version**`) if enough changes (for stable melpa)
   If extra package should append prefix before **version** (which depend on melpa recipe :version-regexp).
3. `git push` && `git push --tags`
