import regex as re
import glob
import os

new_module = 'xmdes'
with open('{}.f90'.format(new_module), 'r') as f:
    lines = f.readlines()

methods = set()
begun = False
for i in range(len(lines)):
    match = re.match(r'^ *(subroutine|function) +(\w+)', lines[i].lower())
    if match:
        methods.add(match.group(2))
        if not begun:
            lines[i] = 'module m_{}\ncontains\n'.format(new_module) + lines[i]
            begun = True
lines[-1] += '\nend module m_{}'.format(new_module)

with open('{}.f90'.format(new_module), 'w') as f:
    f.writelines(lines)

files = [file for file in glob.glob('*.f90') if file != '{}.f90'.format(new_module)]
for file in files:
    with open(file, 'r') as f:
        lines = f.readlines()

    for i in range(len(lines)):
        match = re.match(r'^ *(program|subroutine|function) +(\w+)', lines[i].lower())
        if match:
            j = 0
            brackets = 0
            for j in range(i, len(lines)):
                brackets += lines[j].count('(') - lines[j].count(')')
                if not brackets:
                    break

            for j in range(j + 1, len(lines)):
                match = re.match(r'^( *)\w', lines[j])
                if match:
                    break

            for k in range(j + 1, len(lines)):
                match = re.match(r'^( *)(call|=) +(\w+)', lines[k].lower())
                if match and match.group(3) in methods:
                    lines[j] = '{}use m_{}\n{}'.format(match.group(1), new_module, lines[j])
                    i += k + 1 - i
                    break

    with open(file, 'w') as f:
        f.writelines(lines)
