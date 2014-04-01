## a-cl-logger

A library that separates and extends the arnesi logger.

### Goals

 * minimalist implementation
 * node-logstash integration
 * Swank presentations integration (objects are printed to the REPL as
   an inspectable presentation (C-c M-p)

### Log message format:
 * Either format-string and arguments
 * plist of keys/values

eg:

```
(testlog.debug "Format-string example:#~d" 1)
(testlog.debug :a-plist-key :a-plist-value :some-key "some value")
```