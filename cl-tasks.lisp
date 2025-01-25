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

(define-condition task-finished ()
  ((task :initarg :task :accessor task))
  (:report (lambda (self stream)
             (declare (ignore self))
             (format stream "Task is complete. No more instructions available."))))

(defmethod call ((obj task))
  (with-slots (expr-array index) obj
    (let ((len (length expr-array)))
      (if (< index len)
          (let ((i index))
            (incf index)
            (funcall (aref expr-array i)))
          (signal 'task-finished :task obj)))))
