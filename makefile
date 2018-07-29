all:
	$(MAKE) -C ui
	$(MAKE) -C scraper

live:
	$(MAKE) -C ui live
