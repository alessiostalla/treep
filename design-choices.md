- When installing a language into a Lisp package, we may find previous definitions of the same concepts. We only make the symbols refer to the new concepts, without
   attempting to redefine the classes as DEFCLASS/ENSURE-CLASS would do, because that would require keeping track of the package a language has been installed into,
   making read/load no longer free of side effects. Instead, a separate migration protocol will be designed.
  
