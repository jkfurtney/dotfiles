l_methods = [ "embedded",
              "normal_dir",
              "normal_disp",
              "normal_stiffness",
              "set_normal_stiffness",
              "normal_strength",
              "set_normal_strength",
              "normal_stress",
              "shear_cohesion",
              "set_shear_cohesion",
              "shear_dir",
              "shear_disp",
              "shear_friction",
              "set_shear_friction",
              "shear_residual",
              "set_shear_residual",
              "shear_state",
              "shear_stiffness",
              "set_shear_stiffness",
              "shear_stress",
              "shear_stress",
              "slide",
              "set_slide",
              "slide_tol",
              "set_slide_tol"]

l_classname = "LinerPythonObject"

gg_methods = ["shear_cohesion",
              "set_shear_cohesion",
              "shear_dir",
              "shear_disp",
              "shear_friction",
              "set_shear_friction",
              "shear_state",
              "shear_stiffness",
              "set_shear_stiffness",
              "shear_stress",
              "slide",
              "set_slide",
              "slide_tol",
              "set_slide_tol",
              "stress_confining"]

gg_classname = "GeogridPythonObject"


get_add_template = '    addMethod("{method_name}", (PyCFunction){method_name}, METH_NOARGS, "() -> None.");'
set_add_template = '    addMethod("{method_name}", (PyCFunction){method_name}, METH_VARARGS, "() -> None.");'

decl_template = "    static PyObject *{method_name}(MyObject *self, PyObject *args);"

get_body_template = """  PyObject *{}::{}(MyObject *self, PyObject *) {{
    PYTHON_TRY
    if (!checkSelf(self)) return NULL;
    Py_RETURN_NONE;
    PYTHON_CATCH
  }}
"""

set_body_template = """  PyObject *{}::{}(MyObject *self, PyObject *args) {{
    PYTHON_TRY
    if (!checkSelf(self)) return NULL;
    Py_RETURN_NONE;
    PYTHON_CATCH
  }}
"""

if __name__ == '__main__':
    class_name = l_classname
    methods = l_methods
    for method in methods:
        if method.startswith("set"):
            print set_add_template.format(method_name=method)
        else:
            print get_add_template.format(method_name=method)

    for method in methods:
        print decl_template.format(method_name=method)

    for method in methods:
        if method.startswith("set"):
            print set_body_template.format(class_name,method)
        else:
            print get_body_template.format(class_name,method)
