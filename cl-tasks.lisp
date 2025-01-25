(defclass task ()
  ((expr-array :initarg :expr-array)
   (index)
   (mailbox)))

(defmethod initialize-instance :after ((obj task) &rest rest)
  (declare (ignore rest))
  (with-slots (index) obj
    (setf index 0)))

(defmacro make-task (&rest exprs)
  (let ((array (gensym)))
    `(let ((,array (make-array 0 :adjustable t :fill-pointer 0)))
       ,@(loop for expr in exprs
               collect `(vector-push-extend (lambda () ,expr) ,array))
       (make-instance 'task :expr-array ,array))))
