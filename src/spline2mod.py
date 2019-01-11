import regex as re
import glob
import os

with open('spline.f90', 'r') as f:
    lines = f.readlines()

methods = set()
for line in lines:
    match = re.match(r'^ *(subroutine|function) +(\w+)', line.lower())
    if match:
        methods.add(match.group(2))


files = [file for file in glob.glob('*.f90') if file != 'spline.f90']
for file in files:
    with open(file, 'r') as f:
        lines = f.readlines()

    for i in range(len(lines)):
        match = re.match(r'^ *(subroutine|function) +(\w+)', lines[i].lower())
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
                    lines[j] = '{}use m_spline\n{}'.format(match.group(1), lines[j])
                    i += k + 1 - i
                    break

    with open(file, 'w') as f:
        f.writelines(lines)
