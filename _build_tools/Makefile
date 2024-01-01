run:
	wrangler dev --test-scheduled

db:
	wrangler d1 execute SPECTATOR_DB --local --file=schema.sql

test:
	(cd .. && clear && dune build && clear && dune test -f)

.PHONY: run db test
