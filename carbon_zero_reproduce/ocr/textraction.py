from base import ExtractBase
import textract
from traceback import print_exc

class Textract(ExtractBase):
    def __init__(self, method):
        self.default_method = method

    def extract(self, path):
        if path.endswith('.pdf'):
            try:
                text = textract.process(path, method=self.default_method).decode('utf-8')
                return {-1: {'raw_text':text, 'method': self.default_method}}
            except:
                try:
                    print(f'Path: {path} not readable with pdftotext, try tesseract')
                    text = textract.process(path, method='tesseract').decode('utf-8')
                    return {-1: {'raw_text':text, 'method': 'tesseract'}}
                except:
                    print(f'Path: {path} not readable with pdftotext or tesseract, returning None')
                    print_exc()
        else:
            print(f'Path: {path} has bad extension')
            return None

if __name__ == '__main__':
    Textract.extract('~/personal/carbon_zero_nlp/ocr/London_climate_plan.pdf')
