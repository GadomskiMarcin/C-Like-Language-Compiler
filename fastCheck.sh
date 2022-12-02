RED='\033[0;31m'
NC='\033[0m'
GREEN='\033[0;32m'

rm lattests/good/core0*.myoutput
rm lattests/good/core0*.ll
rm lattests/good/core0*.bc
./latc_llvm lattests/good/core0*.lat
./latc_llvm lattests/extensions/struct/*.lat

echo -e "Test 1"
lli lattests/good/core001.bc > lattests/good/core001.myoutput
diff lattests/good/core001.output  lattests/good/core001.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 2"
lli lattests/good/core002.bc > lattests/good/core002.myoutput
diff lattests/good/core002.output  lattests/good/core002.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 3"
lli lattests/good/core003.bc > lattests/good/core003.myoutput
diff lattests/good/core003.output  lattests/good/core003.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 4"
lli lattests/good/core004.bc > lattests/good/core004.myoutput
diff lattests/good/core004.output  lattests/good/core004.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 5"
lli lattests/good/core005.bc > lattests/good/core005.myoutput
diff lattests/good/core005.output  lattests/good/core005.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 6"
lli lattests/good/core006.bc > lattests/good/core007.myoutput
diff lattests/good/core006.output  lattests/good/core007.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 7"
lli lattests/good/core007.bc > lattests/good/core007.myoutput
diff lattests/good/core007.output  lattests/good/core007.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 8"
lli lattests/good/core008.bc > lattests/good/core008.myoutput
diff lattests/good/core008.output  lattests/good/core008.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 9"
lli lattests/good/core009.bc > lattests/good/core009.myoutput
diff lattests/good/core009.output  lattests/good/core009.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 10"
lli lattests/good/core010.bc > lattests/good/core010.myoutput
diff lattests/good/core010.output  lattests/good/core010.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 11"
lli lattests/good/core011.bc > lattests/good/core011.myoutput
diff lattests/good/core011.output  lattests/good/core011.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 12"
lli lattests/good/core012.bc > lattests/good/core012.myoutput
diff lattests/good/core012.output  lattests/good/core012.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 13"
lli lattests/good/core013.bc > lattests/good/core013.myoutput
diff lattests/good/core013.output  lattests/good/core013.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 14"
lli lattests/good/core014.bc > lattests/good/core014.myoutput
diff lattests/good/core014.output  lattests/good/core014.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 15"
lli lattests/good/core015.bc > lattests/good/core015.myoutput
diff lattests/good/core015.output  lattests/good/core015.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 16"
lli lattests/good/core016.bc > lattests/good/core016.myoutput
diff lattests/good/core016.output  lattests/good/core016.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 17"
echo -e "-37/foo/bar"
lli lattests/good/core017.bc > lattests/good/core017.myoutput
diff lattests/good/core017.output  lattests/good/core017.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 18"
lli lattests/good/core018.bc > lattests/good/core018.myoutput
diff lattests/good/core018.output  lattests/good/core018.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 19"
lli lattests/good/core019.bc > lattests/good/core019.myoutput
diff lattests/good/core019.output  lattests/good/core019.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 20"
lli lattests/good/core020.bc > lattests/good/core020.myoutput
diff lattests/good/core020.output  lattests/good/core020.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 21"
lli lattests/good/core021.bc > lattests/good/core021.myoutput
diff lattests/good/core021.output  lattests/good/core021.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 22"
lli lattests/good/core022.bc > lattests/good/core022.myoutput
diff lattests/good/core022.output  lattests/good/core022.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 23"
lli lattests/good/core023.bc > lattests/good/core023.myoutput
diff lattests/good/core023.output  lattests/good/core023.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 24"
lli lattests/good/core024.bc > lattests/good/core024.myoutput
diff lattests/good/core024.output  lattests/good/core024.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 25"
lli lattests/good/core025.bc > lattests/good/core025.myoutput
diff lattests/good/core025.output  lattests/good/core025.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 26"
lli lattests/good/core026.bc > lattests/good/core026.myoutput
diff lattests/good/core026.output  lattests/good/core026.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 27"
lli lattests/good/core027.bc > lattests/good/core027.myoutput
diff lattests/good/core027.output  lattests/good/core027.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 28"
lli lattests/good/core028.bc > lattests/good/core028.myoutput
diff lattests/good/core028.output  lattests/good/core028.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 31"
lli lattests/good/core031.bc > lattests/good/core031.myoutput
diff lattests/good/core031.output  lattests/good/core031.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test 32"
lli lattests/good/core032.bc > lattests/good/core032.myoutput
diff lattests/good/core032.output  lattests/good/core032.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi

echo -e "Test  Struct - List"
lli lattests/extensions/struct/list.bc > lattests/extensions/struct/list.myoutput
diff lattests/extensions/struct/list.output  lattests/extensions/struct/list.myoutput
ret=$?
if [[ $ret -eq 0 ]]; then
    echo -e "${GREEN}Passed${NC}"
else
    echo -e "${RED}Failed${NC}"
fi
