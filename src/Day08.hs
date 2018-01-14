module Day08 where

import System.Process (callCommand)

solve1_1 = callCommand "\
  \ cat input/input08.txt | \
    \ sed 's/inc/+/g' | \
    \ sed 's/dec/-/g' | \
    \ sed 's/==/=/g' | \
    \ sed 's/!=/-ne/g' | \
    \ sed 's/>=/-ge/g' | \
    \ sed 's/<=/-le/g' | \
    \ sed 's/=/-eq/g' | \
    \ sed 's/>/-gt/g' | \
    \ sed 's/</-lt/g' | \
    \ sed 's/\\([^ ]*\\) \\([^ ]*\\) \\([^ ]*\\) if \\([^ ]*\\) \\(.*\\)/\\1=${\\1:=0}; \\4=${\\4:=0}; if [ \"$\\4\" \\5 ]; then \\1=$(($\\1\\2(\\3))); fi/g' > dist/input.sh; \
  \ echo '' >> dist/input.sh; \
  \ cat input/input08.txt | \
    \ sed 's/\\([^ ]*\\) .*/echo $\\1/g' >> dist/input.sh"

solve1_2 = callCommand "chmod u+x dist/input.sh"

solve1_3 = callCommand "dist/input.sh | sort -gr | head -n1"


solve2_1 = callCommand "\
  \ cat input/input08.txt | \
    \ sed 's/inc/+/g' | \
    \ sed 's/dec/-/g' | \
    \ sed 's/==/=/g' | \
    \ sed 's/!=/-ne/g' | \
    \ sed 's/>=/-ge/g' | \
    \ sed 's/<=/-le/g' | \
    \ sed 's/=/-eq/g' | \
    \ sed 's/>/-gt/g' | \
    \ sed 's/</-lt/g' | \
    \ sed 's/\\([^ ]*\\) \\([^ ]*\\) \\([^ ]*\\) if \\([^ ]*\\) \\(.*\\)/\\1=${\\1:=0}; \\4=${\\4:=0}; if [ \"$\\4\" \\5 ]; then \\1=$(($\\1\\2(\\3))); echo '_'$\\1; fi/g' > dist/input.sh"

solve2_2 = callCommand "chmod u+x dist/input.sh"

solve2_3 = callCommand "dist/input.sh | grep '_.*' | sed 's/_\\(.*\\)/\\1/g' | sort -gr | head -n1"

solution1 = solve1_1 >> solve1_2 >> solve1_3
solution2 = solve2_1 >> solve2_2 >> solve2_3