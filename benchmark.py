#!/bin/env/python3

import sys
import os
import time

if __name__ == "__main__":
    args = sys.argv
    if len(args) == 1:
        print ("first arg is number of loops second is exe")
    else:
        total = 0
        iter = int(args[1])
        for i in range(iter):
            time_pre = time.time()
            os.system("./" + args[2] + "> /dev/null")
            time_post = time.time()
            calc = time_post - time_pre
            total += calc

        print ("File: " + args[2] + ", " + str(iter) + " runs gave average: " + str(total / iter) + "s")
