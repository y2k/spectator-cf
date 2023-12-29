run:
	wrangler dev --test-scheduled

db:
	wrangler d1 execute SPECTATOR_DB --local --file=schema.sql

.PHONY: run db
