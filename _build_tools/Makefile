run:
	wrangler dev --test-scheduled

db:
	wrangler d1 execute SPECTATOR_DB --local --file=schema.sql

test:
	(cd .. && clear && dune build && clear && dune test -f)

hook:
	@NGROK_API="http://localhost:4040/api/tunnels" ; \
	NGROK_URL=$$(curl -s $$NGROK_API | grep -o '"public_url":"[^"]*' | grep -o 'http[^"]*') ; \
	source .dev.vars ; \
	curl "https://api.telegram.org/bot$$TG_TOKEN/setWebhook?max_connections=1&drop_pending_updates=true&url=$$NGROK_URL"

trigger:
	curl http://localhost:8787/__scheduled?cron=*+*+*+*+*

.PHONY: run db test hook trigger
