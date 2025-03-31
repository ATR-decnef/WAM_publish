export USERID="$(id -u)"
export GROUPID="$(id -g)"
export USERNAME="naoyuki_okamoto"

echo $USERID
echo $GROUPID
docker-compose build && docker-compose up -d