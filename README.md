## a-cl-logger

A common lisp logging library providing context sensitive logging of
more than just strings to more than just local files / output streams

This started as a significant refactor arnesi logging and the many
chunks of surrounding code.

### Goals

 * node-logstash integration
 * Swank presentations integration (objects are printed to the REPL as
   an inspectable presentation (C-c M-p)
 * Support logging of more than just strings, eg: json
 * Context sensitive logging (easily use the dynamic context to add to 
   the data being logged)
 * Gracefully handle slime/swank reconnects
 * Errors in logging shouldnt cause application errors, but should
   still be debuggable

### Quickstart

```

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :a-cl-logger))

;; log to stderr
(a-cl-logger:define-logger testlog ())
(defun do-something(x)
  (testlog.debug "starting to do something to ~a" x)
  (testlog.info "did something to ~a" x))

;; log to stderr and a file (see note about buffering in "Gotchas" below)
(a-cl-logger:define-logger filelog ()
  :appenders (make-instance 'a-cl-logger:file-log-appender :log-file "test.log"))

```

### Description and Glossary 

A Logger is a mechanism for generating a text message and have that
messaged saved somewhere for future review. Logging can be used as a
debugging mechanism or for just reporting on the status of a system.

Messages are sent to a particular Logger. Each logger sends the
messages it receives to its appenders. An appenders's job is to take a
message and write it somewhere. Loggers are organized in a hierarchy
and messages sent to a logger will also be sent to that loggers's
parents (if valid). Only if the message is enabled for the current
logger will it be propogated to the parents.

Each logger has a level which is used to determine whether a particular
message should be processed or not. Loggers inherit their log level from
their parents. If a logger has multiple direct parents its log level is
the min of the levels of its parents.

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

### Root Logger

There is a root logger which all other loggers have as a parent by
default.  This is a conveneint place to put appenders that should
always apply.  You can remove the root by removing it from the parents
slot of a logger.

### Signals and Restarts - Context Sensitive Logging

Messages generate signals on being created and on being appended.

 * generating-message - signaled when a message is created
 * logging-message - signaled when a specific logger begins handling a
   message
 * appending-message - signaled when a specific appender begins
   handling a message


#### Muffling log messages / aborting

Each signal can be aborted which will prevent the operation from
occuring.

Aborting while appending will prevent the message from going to a
single destination.  Aborting while logging will prevent the message
from going to ANY appenders of the log or its parents.  Aborting while
generating a message just muffles that log message entirely

#### Changing / Adding to the messages being logged

At each of these points you can invoke the restart `change-message` to
alter the message going out.  Generally the message you change to will
be a copy of the original (see copy-message).

Each of these signals are wrapped in a `change-message` restart that
can be used to modify the message for the remainder of the operation.
(IE: a specific appender will operate on a new copy of the message
with different, supplemental data).

#### Default Handlers

Signals can be handled by handler-binds on the dynamic context or by
adding default-handlers to the `default-signal-bindings` slot on the
logger.  This can be accomplished using `add-signal-handler` passing 
the logger, a condition type and a fn.

Here are some examples. The first prevents the timing log from being inserted
into the log file.  The second prevents a particular log message from 
being inserted into the logs.

```
(defun mute-timing-file-appends (c)
    (let* ((m (a-log:message c))
           (l (a-log:logger m))
           (a (a-log:appender c)))
      (when (and (typep a 'a-log:file-log-appender)
                 (eql l *timing-log*))
        (abort c))))

(a-log:add-signal-handler
 *timing-log* 'a-log:appending-message 'mute-timing-file-appends)

(defun ignore-foreach-warnings (c)
  (let ((fs (a-log:format-control (a-log:message c))))
    (when (cl-ppcre:scan #?r"(?i)invalid argument supplied for foreach" fs)
      (abort c))))

(a-log:add-signal-handler
 *wp-log* 'a-log:generating-message 'ignore-foreach-warnings)

```

### Helper Functions

 * `get-log-fn`: given a logger and an optional level create a function
   of &rest args that logs to the given logger. Useful for interacting
   with libraries providing functional logging hooks
 
 * `with-appender`: create a dynamic scope inside which messages to
   logger will additionally be appended to this appender

 * `when-log-message-generated/logged/appended`: These are macros that
   establish a dynamic context inside of which log messages will be
   intercepted at key points in their life cycle.  The first body is 
   the message handler and the second is the scope.
 
 * `setup-logger`: a function that will ensure log-level and standard
   debug-io-appender / file-appenders are in place.  Useful when debug
   IO become rebound etc (slime session resets).  Probably not as
   useful now that there is a root logger and we dont have to constantly 
   attach the same appenders everywhere

### Gotchas

 * There are some SBCL specifics.  Cross platform help would be nice
  * "--quiet" command line arg
  * logstash hostname 
 * file log appender output may be buffered by the lisp
   implementation's stream operations. If you have a slow process,
   messages may take some time to be written to the log file. Use
   `(make-instance 'a-cl-logger:file-log-appender :log-file "test.log"
   :buffer-p nil)` to aggressively call
   [force-output](http://l1sp.org/cl/force-output) and ensure the log
   file is accurate.

### Differences From Arnesi/src/log.lisp

 * There has been some significant renaming
  * deflogger -> define-logger
  * log-category -> logger
  * appenders are separate from formatters
 * File streams ensure the file is open to write to
 * Failing to write to one appender / logger doesnt prevent the rest
   from working

## Examples:
!! These examples are all in SBCL !!

### Filtering message content

This will abort / mute log messages that match a certain warning
```
(defun ignore-foreach-warnings (c)
  (let ((fs (a-log:format-control (a-log:message c))))
    (when (cl-ppcre:scan #?r"(?i)invalid argument supplied for foreach" fs)
      (abort c))))

(define-logger my-module-log (my-log))

```

#### Filter log messages globally for a specific logger:

```
(a-log:add-signal-handler
 *my-module-log* 'a-log:generating-message 'ignore-foreach-warnings)
```

#### Filter log messages locally for a logger:

```
(defmacro with-muted-foreach-warnings (() &body body)
  (handler-bind ((a-log:generating-message #'ignore-foreach-warnings))
    (progn ,@body)))
```

### Basic Context Sensitive Logging

(a-log:when-log-message-generated
    ((a-log:push-into-message :a 1 :b 2))
  (log.debug "This message will have a=1 and b=2 in its data"))


### Logging the lexical environment

```

(a-log:define-logger my-log ()
  :appenders (make-instance 'a-log:stream-log-appender :stream *standard-output*))

(defun compile-env-data-list (env)
  (let* ((blocks (mapcar #'first (reverse (sb-c::lexenv-blocks env))))
         (lex-vars (reverse (sb-c::lexenv-vars env)))
         (vars (iter (for (name . v) in lex-vars)
                 (for ignore? = (ignore-errors (sb-c::lambda-var-ignorep v)))
                 (unless ignore?
                   (collect `(quote ,name))
                   (collect name)))))
    (values
     (first blocks)
     `(list :blocks (princ-to-string '(,@blocks))
       (list ,@vars)))))

(defmacro with-env-stats-recorder (()
                                   &body body &environment env)
  "Signals the realtime of the body, pulling data and tag from the lexical env"
  ;; NB: To debug this you will need to break in here and inspect body
  ;; macroexpand doesnt work
  (multiple-value-bind (name data-forms)
      (compile-env-data-list env)
    `(a-log:when-log-message-generated
      ((a-log:push-into-message :lexical-data (,@data-forms) :name ',name))
      ,@body)))

(defun test-cs-logging (&key (a 1) (b 2))
  (my-log.info "OUT of Env Recorder ")
  (with-env-stats-recorder ()
    (my-log.info "In test-cs-logger recorder ")
    (let ((c (+ 1 b)))
      (flet ((again (d)
               (incf d)
               (with-env-stats-recorder ()
                 (my-log.info "In again ~a " d))))
        (again a)
        (again b)))))
```

