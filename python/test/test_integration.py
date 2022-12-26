import pytest
import os


def test_lineal():
    stream = os.popen('cat data/test1.txt | hs-lab3-exe -l 1 -r 10 -s 1 -c 2 -m l')
    res = stream.read()
    file = open("data/test1_output.txt", "r")
    excepted = file.read()
    assert(res == excepted)

