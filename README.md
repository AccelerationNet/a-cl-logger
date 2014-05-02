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

 * node-logstash integration
 * Swank presentations integration (objects are printed to the REPL as
   an inspectable presentation (C-c M-p)
 * Support logging of more than just strings, eg: json
 * Context sensitive logging (easily use the dynamic context to add to 
   the data being logged)

### Glossary

 * Logger - an object that contains a list of places to send messages
   (appenders) and a level.  Loggers are hierarchical and will send 
   messages to all parent appenders and defer to parent levels.
 * Level - dribble, debug, info, warn, error, fatal, a name and number
   indicating the sevarity of log messages.  Levels are used to filter
   which messages are sent to which loggers and appenders
 * Appenders - an object that transmits a message to a location (file,
   repl, node-logstash) in a given format (by using the formatter
   found in the formatter slot)
 * Formatter - format-message is specialized on a formatter, which is
   stored on each appender.
 * Message - The data trying to be logged, a localtime:timestamp, and
   a level indicating this message's importance

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

#### Helper macros ####

Helper 

```
(testlog.debug "Format-string example:#~d" 1)
(testlog.info :a-plist-key :a-plist-value :some-key "some value")
```

These helper macros will handle errors in log argument evaluation (you
can still debug if *debug-hook* is bound). They will also capture the
literal arguments provided to the macro to ease debugging.

#### do-log ####
The `do-log` function can also be used to create log messages

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

 * generating-message - signaled when a message is created
 * logging-message - signaled when a specific logger begins handling a
   message
 * appending-message - signaled when a specific appender begins
   handling a message

Each of these signals are wrapped in a `change-message` restart that
can be used to modify the message for the remainder of the operation.
(IE: a specific appender will operate on a new copy of the message with
different, supplemental data).

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