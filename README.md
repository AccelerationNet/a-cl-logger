## a-cl-logger

A library that separates and extends the arnesi logger.

### Description and Glossary 

A Logger is a mechanism for generating a text message and have that
messaged saved somewhere for future review. Logging can be used as a
debugging mechanism or for just reporting on the status of a system.

Messages are sent to a particular Logger. Each logger sends the messages
it receives to its appenders. An appenders's job is to take a message and
write it somewhere. Loggers are organized in a hierarchy and messages
sent to a logger will also be sent to that loggers's parents.

Each logger has a level which is used to determine whether are particular
message should be processed or not. Loggers inherit their log level from
their ancestors. If a logger has multiple direct parents its log level is
the min of the levels of its parents.

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