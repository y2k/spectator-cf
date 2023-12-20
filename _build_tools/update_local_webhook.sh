WEBHOOK_URL="$1"
source .dev.vars
echo "${WEBHOOK_URL}"
curl -F "max_connections=1" -F "drop_pending_updates=true" -F "url=${WEBHOOK_URL}" https://api.telegram.org/bot${TG_TOKEN}/setWebhook
