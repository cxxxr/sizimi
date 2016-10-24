#!/bin/sh

../roswell/sizimi.ros < script > "/tmp/$$"
diff "/tmp/$$" result
rm "/tmp/$$"
