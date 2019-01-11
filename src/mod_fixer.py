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

# Find out which modules are ACTUALLY used by functions
actually_using = dict()
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
            actually_using.update({current_function: set()})
            continue

        if current_function is not None:
            match = re.match(r'^ *call +(\w+)', lines[i].lower())
            if not match:
                match = re.search(r'= +(\w+)\(', lines[i].lower())

            if match:
                func = match.group(1)
                for module, funcs in functions.items():
                    if func in funcs:
                        actually_using[current_function].add(module)

# Find out which functions declare to use which modules without actually using them
not_using = dict()
for function, using in declared_using.items():
    for use in using:
        if use not in actually_using[function]:
            if function not in not_using:
                not_using.update({function: set()})
            not_using[function].add(use)


# Remove using declarations which are not actually used
for file in files:
    with open(file, 'r') as f:
        lines = f.readlines()

    with open(file, 'w') as f:
        current_function = None
        for i in range(len(lines)):
            if current_function is not None and current_function in not_using:
                match = re.match(r'^ *use (\w+)', lines[i].lower())
                if match and match.group(1) in not_using[current_function]:
                    continue

            match = re.match(r'^ *(function|subroutine) +(\w+)', lines[i].lower())
            if match:
                current_function = match.group(2)

            f.write(lines[i])




exit(0)
