shell_methods = ["shell_area",
                 "shell_depth_factor",
                 "shell_element_type",
                 "shell_force_nodal",
                 "shell_poisson",
                 "shell_pressure",
                 "shell_prop_anis",
                 "shell_prop_ortho",
                 "shell_prop_type",
                 "shell_resultant",
                 "shell_resultant_valid",
                 "shell_stress",
                 "shell_stress_prin",
                 "shell_stress_valid",
                 "shell_thickness",
                 "shell_volume",
                 "shell_young"]

shell_classname = "ShellPythonObject"

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
    class_name = shell_classname
    methods = shell_methods
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
