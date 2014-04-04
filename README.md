## a-cl-logger

A library that separates, extends and refactors the arnesi logger.

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
their parents. If a logger has multiple direct parents its log level is
the min of the levels of its parents.

### Goals

 * minimalist implementation
 * node-logstash integration
 * Swank presentations integration (objects are printed to the REPL as
   an inspectable presentation (C-c M-p)
 * Support logging of more than just strings, eg: json
 * Context sensitive logging (easily use the dynamic context to add to 
   the data being logged)

### Log message format:

Messages contain:
 * format-string and arguments
 * plist of keys/values
 * logger
 * level

The default printing of a message is  
"{ts} {logger}{level} formatted-msg {key,val ...}" 

eg:

```
(testlog.debug "Format-string example:#~d" 1)
(testlog.debug :a-plist-key :a-plist-value :some-key "some value")
```

### Logging

Logging can be accomplished by a couple of means:

Helper macros are created like:

```
(testlog.debug "Format-string example:#~d" 1)
(testlog.info :a-plist-key :a-plist-value :some-key "some value")
```

The `do-log` function can also be used

```
(do-log *testlog* +info+ "Format-string example:#~d" 1)
(do-log 'testlog +debug+ :a-plist-key :a-plist-value :some-key "some value")
```

There is also a helper get-log-fn which will create a function of
(&rest args) that logs to a given logger and level).  This is useful
for libraries that supply functional logging hooks

```
(get-log-fn *testlog* :level +info+) => (lambda (&rest args) ...)
```

### Changing / Adding to the messages being logged

Messages generate signals on being created and on being appended.
At each of these points you can invoke the restart `change-message`
to alter the message going out.  Generally the message you change 
to will be a copy of the original (see copy-message).

### Gotchas

 * There are some SBCL specifics.  Cross platform help would be nice
  * "--quiet" command line arg
  * logstash hostname 

### Differences From Arnesi/src/log.lisp
 * There has been some significant renaming
  * deflogger -> define-logger
  * log-category -> logger
  * appenders are separate from formatters
 * File streams ensure the file is open to write to
 * Failing to write to one appender / logger doesnt prevent the rest
   from working