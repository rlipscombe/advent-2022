def windowed(input; len):
    reduce range(0; ($input | length) - $len) as $i ([]; . + [$input[$i:$i+$len]]);

def str_unique: explode | unique | implode;
def all_unique: length as $len | select((. | str_unique | length) == $len);

$input | index([windowed($input; $len) | .[] | select(all_unique)] | .[0]) + $len
