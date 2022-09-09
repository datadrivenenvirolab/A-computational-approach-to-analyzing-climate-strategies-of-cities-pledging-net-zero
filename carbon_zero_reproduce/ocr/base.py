class ExtractBase:

    def extract(path: str) -> dict:
        """Takes a path to a pdf and produces extracted text

        Output is a dictionary mapping from page number to text,
        and other metadata. -1 maps to all of the text."""
