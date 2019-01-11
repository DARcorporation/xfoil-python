import regex as re
import glob


# Create a dict of modules and their functions and which modules each function uses
functions = dict()
declared_using = dict()
files = [file for file in glob.glob('*.f90')]
for file in files:
    with open(file, 'r') as f:
        lines = f.readlines()

    current_module = None
    current_function = None
    for i in range(len(lines)):
        match = re.match(r'^ *module (\w+)', lines[i].lower())
        if match:
            current_module = match.group(1)
            functions.update({current_module: set()})
            continue

        match = re.match(r'^ *(function|subroutine) +(\w+)', lines[i].lower())
        if match:
            current_function = match.group(2)
            declared_using.update({current_function: set()})

            if current_module is not None:
                functions[current_module].add(current_function)
            continue

        if current_function is not None:
            match = re.match(r'^ *use (\w+)', lines[i].lower())
            if match and not match.group(1).startswith('i_'):
                declared_using[current_function].add(match.group(1))


# Remove modules without functions
for key in list(functions.keys()):
    if not len(functions[key]):
        del functions[key]

# Remove functions without using
for key in list(declared_using.keys()):
    if not len(declared_using[key]):
        del declared_using[key]

# Find out which module functions specifically are used by functions
only_using = dict()
for file in files:
    with open(file, 'r') as f:
        lines = f.readlines()

    current_module = None
    current_function = None
    for i in range(len(lines)):
        match = re.match(r'^ *module (\w+)', lines[i].lower())
        if match:
            current_module = match.group(1)
            continue

        match = re.match(r'^ *(function|subroutine) +(\w+)', lines[i].lower())
        if match:
            current_function = match.group(2)
            only_using.update({current_function: dict()})
            continue

        if current_function is not None:
            match = re.match(r'^ *call +(\w+)', lines[i].lower())
            if not match:
                match = re.search(r'= +(\w+)\(', lines[i].lower())

            if match:
                func = match.group(1)
                for module, funcs in functions.items():
                    if func in funcs:
                        if module not in only_using[current_function]:
                            only_using[current_function].update({module: set()})
                        only_using[current_function][module].add(func)

# Remove functions without using
for key in list(only_using.keys()):
    if not len(only_using[key]):
        del only_using[key]

# Specify exactly which functions are used from modules
for file in files:
    with open(file, 'r') as f:
        lines = f.readlines()

    with open(file, 'w') as f:
        current_function = None
        for i in range(len(lines)):
            match = re.match(r'^ *(function|subroutine) +(\w+)', lines[i].lower())
            if match:
                current_function = match.group(2)

            if current_function is not None and current_function in only_using:
                match = re.match(r'^ *use (\w+)', lines[i].lower())
                if match and match.group(1) in only_using[current_function]:
                    lines[i] = lines[i][:-1] + \
                               ', only: {}'.format(', '.join(only_using[current_function][match.group(1)])) + '\n'

            f.write(lines[i])

exit(0)
