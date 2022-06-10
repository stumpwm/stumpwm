# dynamic-mixins

Dynamic-mixins is for simple, dynamic class combination:

```lisp
(in-package :dynamic-mixins)

(defclass a () ())
(defclass b () ())
(defclass c () ())

(make-instance (mix 'a 'b)) ;; => #<MIXIN-OBJECT (A B)>

(let ((a (make-instance 'a)))
  (ensure-mix a 'b 'c)      ;; => #<MIXIN-OBJECT (B C A)>
  (delete-from-mix a 'a)    ;; => #<MIXIN-OBJECT (B C)>
  (delete-from-mix a 'c))   ;; => #<B>
```

This allows objects to be mixed and updated without manually
defining many permutations.

## Dictionary

* `MIX &rest classes`: This produces a "mix
  list", which is generally only useful for passing to
  `MAKE-INSTANCE`.  Note: Order matters!  This determines class
  precedence.

* `ENSURE-MIX object &rest name-or-class`: Ensure that classes listed
  in `name-or-class` are part of `object`.  This will create a new
  class and `CHANGE-CLASS object` if necessary.  Note: Order matters!
  This determines class precedence.

* `DELETE-FROM-MIX object &rest name-or-class`: Remove classes listed
  in `name-or-class` from the object's class.  This will create a new
  class and `CHANGE-CLASS object` if necessary.  However, `object`
  must be a `MIXIN-OBJECT` created by `(MAKE-INSTANCE (MIX ...) ...)`
  or `ENSURE-MIX`.  Otherwise, nothing will be changed.

## Notes

### Order and Precedence

Order matters; you are defining a new class which has the specified
classes as direct superclasses.

`ENSURE-MIX` *prepends* classes in the order specified.  (Originally,
it appended classes.)  This is simply more useful in practice:

```lisp
(defclass general-object () ())
(defclass specializing-mixin () ())

(defgeneric some-operation (x))

(defmethod some-operation (x)
   "Handle the general case"
   ...)

(defmethod some-operation ((x specializing-mixin))
   "Handle the case for SPECIALIZING-MIXIN"
   ...)

(let ((x (make-instance 'general-object)))
  (ensure-mix x 'specializing-mixin)
  (some-operation x))
```

If `SPECIALIZING-MIXIN` were appended, the method which specialized on
it would never be called.  In practice, this defeats the point.
Therefore, mixins now get precedence.

### Errors

Errors regarding precendence and circularity are now handled, or
rather, causing such an error will not produce a nearly-unrecoverable
situation.  Now you will just get an error.
