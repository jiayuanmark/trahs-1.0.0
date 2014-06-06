#!/usr/bin/env bash
# Simple trahs test script.

TRA=$(pwd)/dist/build/trahs/trahs
TESTDIR=t
T1=a
T2=b
EXP=expected

export TRASSH="${TRA} --server"

function dir_match_no_conflicts {
  for f in $(ls "$2" | grep -v "#"); do
    diff -q "${2}/${f}" "${3}/${f}"
    if [ $? != 0 ]; then
      echo "$1 failed!"
      exit $?
    fi
  done
}

function dir_match {
  for f in $(ls "$2"); do
    diff -q "${2}/${f}" "${3}/${f}"
    if [ $? != 0 ]; then
      echo "$1 failed!"
      exit $?
    fi
  done
}

function dirs_match_no_conflicts {
  dir_match_no_conflicts "$1" "$2" "$3"
  dir_match_no_conflicts "$1" "$3" "$2"
}

function dirs_match {
  dir_match "$1" "$2" "$3"
  dir_match "$1" "$3" "$2"
}

function run_tra {
  BASE=`pwd`
  $TRA "_:${BASE}/${T1}" "${BASE}/${T2}" 2> /dev/null
}

function validate_test {
  dirs_match "$1" "${T1}" "${T2}"
  dirs_match "$1" "${T1}" "${EXP}"
}


# clean
mkdir -p ${TESTDIR}/${T1}
mkdir -p ${TESTDIR}/${T2}

cd ${TESTDIR}

# test 1 -- fresh (new) sync -- no conflicts
cat <<EOF > ${T1}/file1
1_line_1
1_line_2
EOF

cat <<EOF > ${T1}/file2
2_line_1
EOF

touch ${T1}/file3

cp -r ${T1} ${EXP}

run_tra
validate_test "test 1 -- fresh"

# test 2 -- delete a file in b
rm b/file3
rm expected/file3

run_tra
validate_test "test 2 -- delete"

# test 3 -- new file in a, new file in b
echo "4_line_1" >> ${T1}/file4
echo "5_line_1" >> ${T2}/file5
cp ${T1}/file4 ${EXP}
cp ${T2}/file5 ${EXP}

run_tra
validate_test "test 3 -- new files"

# test 4 -- update existing file
echo "4_line_2" >> ${T1}/file4
echo "4_line_2" >> ${EXP}/file4

run_tra
validate_test "test 4 -- update files"

# test 5 -- existing db to fresh db
rm -r ${T2}/.trahs.db
rm ${T2}/*

run_tra
validate_test "test 5 -- existing to fresh db"

# test 6 -- conflict
echo "5_line_2" >> ${T1}/file5
echo "5_line_3" >> ${T2}/file5
rm ${EXP}/file5

run_tra
dirs_match "test 6 -- conflict" "${T1}" "${T2}"
dirs_match_no_conflicts "test 6 -- conflict" "${T1}" "${EXP}"

if [ $(ls ${T1} | grep "file5#" | wc -l) != 2 ]; then
  echo "test 6 failed!"
  exit 1
fi

cd ..
rm -r ${TESTDIR}
echo "Passed!"

