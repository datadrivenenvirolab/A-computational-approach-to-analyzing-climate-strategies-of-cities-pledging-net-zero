from textraction import Textract
import argparse
import os
from tqdm import tqdm
import json

EXTRACTOR_CLASS = Textract(method='tesseract')
PDF_FOLDER_PATH = '/Users/siddharthsachdeva/personal/carbon_zero_nlp/data/all_netzero_data_09062020'
OUT_FOLDER_PATH = '/Users/siddharthsachdeva/personal/carbon_zero_nlp/data/all_netzero_data_textracttesseract20200906'

if __name__ == '__main__':
    all_pdf_paths = os.listdir(PDF_FOLDER_PATH)
    for fname in tqdm(all_pdf_paths):
        path = os.path.join(PDF_FOLDER_PATH, fname)
        pdf_json = EXTRACTOR_CLASS.extract(path)
        new_fname = fname.replace('.pdf', '_tesseract20200906.json')
        new_path = os.path.join(OUT_FOLDER_PATH, new_fname)
        with open(new_path, 'w') as out_path:
            json.dump(pdf_json, out_path)
