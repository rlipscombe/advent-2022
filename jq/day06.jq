def windowed(input; len):
    reduce range(0; ($input | length) - $len) as $i ([]; . + [$input[$i:$i+$len]]);

def str_unique: explode | unique | implode;
def all_unique: length as $len1 | (str_unique | length) as $len2 | select($len1 == $len2);

$input | ([windowed($input; $len) | .[] | select(all_unique)] | .[0]) as $needle | index($needle) + $len
