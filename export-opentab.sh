if [ -z "$1" ]; then
  echo "Usage: export-opentab.sh <db.sqlite3>"
fi

sqlite3 -bail "$1" \
  'select debater.first_name, debater.last_name, debater_remote_info.remote_id, debater.registration_key from debater join debater_remote_info on debater.id = debater_remote_info.debater_id;'
