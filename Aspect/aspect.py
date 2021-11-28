import json
import numpy as np
import pandas as pd
import nltk

with open('tacobell_reviews.json', encoding='utf8') as f1:
    tacobell_reviews = json.load(f1)
with open('chipotle_reviews.json', encoding='utf8') as f2:
    chipotle_reviews = json.load(f2)


def extract_aspects(reviews):
    """
    从一个business的review中抽取aspects
    """

    aspects_dic = {}
    for review_data in reviews:
        sentence = review_data['text']
        if sentence == None or str.strip(sentence) == '':
            continue
        tagged_words = []
        tokens = nltk.word_tokenize(sentence)
        tag_tuples = nltk.pos_tag(tokens)

        for (word, tag) in tag_tuples:
            if tag == "NN":
                # token = {'word': string, 'pos': tag}
                # tagged_words.append(word)
                if word not in aspects_dic:
                    aspects_dic[word] = []
                aspects_dic[word].append(review_data['review_id'])
    # print(aspects_dic)

    # 对字典进行排序
    aspects_sorted = sorted(aspects_dic.items(), key=lambda x: len(x[1]), reverse=True)
    aspects_dic = {}
    aspect_filter = []
    for index, item in enumerate(aspects_sorted):
        if item[0] in aspect_filter:
            continue

        if len(aspects_dic.items()) < 5:
            aspects_dic[item[0]] = item[1]
    return aspects_dic


def is_review_only_one_aspect(review_text):
    # 判断评论里面是否只包含一个方面
    tagged_words = []
    tokens = nltk.word_tokenize(review_text)
    tag_tuples = nltk.pos_tag(tokens)
    for (word, tag) in tag_tuples:
        if tag == "NN":
            tagged_words.append(word)
    if len(tagged_words) <= 1:
        return True
    return False


def get_cur_aspect_adj(text):
    tokens = nltk.word_tokenize(text)
    tag_tuples = nltk.pos_tag(tokens)
    for (word, tag) in tag_tuples:
        if tag == "JJ" or tag == "ADJ":
            return word
    return None


def get_segment(review_text, Aspects, default=''):
    res = []

    # if is_review_only_one_aspect(review_text):
    #     res.append(review_text)

    def get_all(review_text, aspect, default):
        if aspect not in review_text:
            return ""

        cur_aspect_index = review_text.index(aspect)
        temp = cur_aspect_index

        if temp != 0:
            cur_aspect_index -= 2

            while review_text[cur_aspect_index] not in ',.!?;\n' and cur_aspect_index >= 0:
                cur_aspect_index -= 1
            if review_text[cur_aspect_index + 1] == " ":
                cur_aspect_index += 2
            else:
                cur_aspect_index += 1

        cur_aspect_end_index_begin = temp + len(aspect)
        cur_aspect_end_index_end = cur_aspect_end_index_begin
        end_pos = len(review_text) - 1

        stop_punct_map = {c: None for c in ',.!?;\n'}
        relation_punct_list = ["and", "when", "but"]

        # next_aspect = self.get_next_aspect(review_text[cur_aspect_end_index_begin:end_pos])
        cur_aspect_des = get_cur_aspect_adj(review_text[cur_aspect_end_index_begin:end_pos])

        while cur_aspect_end_index_end <= end_pos:
            # 在标点符号处截取
            cur_str = review_text[cur_aspect_end_index_end:min(cur_aspect_end_index_end + 1, end_pos)]
            if cur_str in stop_punct_map:
                break

            # 在转移符号处截取
            cur_strs = review_text[cur_aspect_end_index_begin:cur_aspect_end_index_end]
            relation_store = ""
            for relation in relation_punct_list:
                if relation in cur_strs.lower():
                    relation_store = relation
                    break

            if relation_store != "":
                cur_aspect_end_index_end -= len(relation_store)
                break

                # 在aspect最近的形容词截取
            if cur_aspect_des is not None:
                if cur_aspect_des in cur_strs:
                    break

            cur_aspect_end_index_end += 1

        cur_aspect_end_index_end = min(cur_aspect_end_index_end, end_pos)

        aim = review_text[cur_aspect_index:cur_aspect_end_index_end]
        sentence = aim
        for item in default:
            sentence = sentence.replace(item, '')

        if aspect in sentence and aim not in res:
            res.append(aim)

        text = review_text[cur_aspect_end_index_end:]
        get_all(text, aspect, default)

    for words in Aspects:
        get_all(review_text, words, default)
        for item in res:
            review_text = review_text.replace(item, "")

    return res


print(chipotle_reviews[0]['text'])

default = ['fast food', 'food chains', 'taco bell', 'making the food', 'order food']
Aspects = ['food', 'burrito', 'taco', 'chipotle', 'rice', 'bowl', 'chicken', 'beans', 'salsa',
           'chips', 'meat', 'guacamole', 'steak', 'cheese', 'sour cream', 'lettuce', 'corn',
           'salad', 'ingredient', 'delicious', 'quality','taste']

tacobell_reviews_food = []
for r in tacobell_reviews:
    seg = get_segment(r['text'], Aspects, default)
    if len(seg) > 0:
        tacobell_reviews_food.append(seg)

chipotle_reviews_food = []
for r in chipotle_reviews:
    seg = get_segment(r['text'], Aspects, default)
    if len(seg) > 0:
        chipotle_reviews_food.append(seg)

from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer

analyzer = SentimentIntensityAnalyzer()

tacobell_food_score = []
for sentences in tacobell_reviews_food:
    for sentence in sentences:
        vs = analyzer.polarity_scores(sentence)
        info = [vs.get('neg'), vs.get('pos'), vs.get('neu'), vs.get('compound')]
        tacobell_food_score.append(info)

chipotle_food_score = []
for sentences in chipotle_reviews_food:
    for sentence in sentences:
        vs = analyzer.polarity_scores(sentence)
        info = [vs.get('neg'), vs.get('pos'), vs.get('neu'), vs.get('compound')]
        chipotle_food_score.append(info)


taco_food = pd.DataFrame(tacobell_food_score)

chip_food = pd.DataFrame(chipotle_food_score)

taco_food.to_csv("taco_food2.csv", index=False, sep=',')
chip_food.to_csv("chip_food2.csv", index=False, sep=',')

pd.DataFrame(tacobell_reviews_food).to_csv("taco_reviews_food.csv", index=False, sep=',')
pd.DataFrame(chipotle_reviews_food).to_csv("chip_reviews_food.csv", index=False, sep=',')
