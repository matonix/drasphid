scrape:
	$(MAKE) -C scraper scrape

all:
	$(MAKE) -C ui
	$(MAKE) -C scraper

live:
	$(MAKE) -C ui live
