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
    review_text = review_text.lower()
    res = []
    c_score = []

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

                # 在下一个aspect截取
                # if next_aspect != None:
                # 	if next_aspect in aspects_dic and next_aspect in cur_strs:
                # 		cur_aspect_end_index_end -= len(next_aspect)
                # 		break

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
            text_tokens = word_tokenize(aim)
            tokens_without_sw = [word for word in text_tokens if word not in stopwords]
            aim = (" ").join(tokens_without_sw)
            vs = analyzer.polarity_scores(aim)
            if vs.get('compound') != 0:
                print(vs.get('compound'))
                res.append(aim)
                c_score.append(float(vs.get('compound')))

        text = review_text[cur_aspect_end_index_end:]
        get_all(text, aspect, default)

    for words in Aspects:
        get_all(review_text, str(words), default)
        for item in res:
            review_text = review_text.replace(item, "")

    if len(c_score) > 0:
        compound = sum(c_score)/len(c_score)
        return [res, compound]


concern = 'food'
'''
FOOD:

default = ['fast food', 'food chains', 'taco bell', 'making the food', 'order food']
Aspects = ['food', 'bowl', 'burrito', 'taco', 'rice', 'chicken', 'beans', 'salsa', 'chips', 'meat',
           'steak', 'cheese', 'sour cream', 'lettuce', 'corn', 'salad', 'ingredient', 'delicious', 'taste',
           'black', 'steak', 'guac', 'sauce', 'fresh', 'veggie', 'cilantro', 'stale', 'cooked', 'salty', 'portion',
           'amount', 'size','tortilla', 'scoop', 'meal', 'lunch', 'water']
'''
'''
SERVICE:
'''
default = ['fast food', 'fast-food', 'am wrong', 'can\'t go wrong', 'I\'m wrong', 'I was wrong']
Aspects = ['service', 'time', 'attitude', 'staff', 'friendly', 'welcoming', 'manager', 'supervisor',
            'helpful', 'employee', 'polite', 'fast', 'customer', 'train', 'rude', 'wrong', 
            'serving', 'line', 'girl', 'guy', 'experience', 'cashier', 'lady','worker', 'minute', 'wait','online',
            'ready','slow','am','pm', 'pickup','hour','management','gloves', 'hand','app','deliver','missing',
            'card','phone', 'quick', 'busy', 'counter']#dinnning

'''
PRICE:
default = ['costume', 'at all cost','accosted']
Aspects = ['price', 'cheap', 'affordable', 'pricy', 'expensive', 'cost', 'free', 'refund']
'''
'''
AMBIANCE:
default = ['encountered']
Aspects = ['ambiance', 'environment', 'atmosphere', 'dirty', 'filthy', 'seat', 'music', 'wifi', 'window',
            'floor', 'decor', 'door', 'table', 'clean','trash','gloves','disgusting','mess','parking',] #crowd
'''

# text = "I want to be clear in the review. I LOVE chipotles food and i give it 5 stars. \nBut i want to review this location. The most of the staff SUCKS. Especially the manager guy (shaved hair, usually wear a black tshirt). \n\nHe is aggressive toward the customers. One one visit my bf couldn't hear what the girl on the line was asking him so he leaned over a little bit and asked \"what?\" all the sudden the manager guy was staring at him angrily asking if there was a problem. And then continued to stare as we checked out. He is lucky I didn't jump over the counter and ask him if he needed that look taken off his face. To top it off the food was sub par as to what we are used to. \n\nWe didn't visit chipotle for a while and we did again last week. The same guy is still the manager and while he didn't give us any problems, I saw his approach another guest who got lemonade in a water cup. Now I understand you shouldn't be saying you want water and getting a different drink but the manner in which he approached this guy was aggressive. Again the food was bad, my barbacoa tasted like soap and they have lessen the already pitiful amount of chips they give you. Seeing other reviews I see this location needs to GET IT TOGETHER. \n*oh and the parking does suck but i know there isnt much they can do about it."
# print(get_segment(text, Aspects, default))


find1 = tacobell_reviews
# tacobell_reviews_sub = [0 for r in find1]
tacobell_reviews_bid = [r['business_id'] for r in find1]
tacobell_reviews_date = [r['date'] for r in find1]
tacobell_reviews_star = [r['stars'] for r in find1]
tacobell_sep_score = [[None for asp in Aspects] for r in find1]
for j in range(len(find1)):
    temp = [None for asp in Aspects]
    for i in range(len(Aspects)):
        seg = get_segment(find1[j]['text'], [Aspects[i]], default)
        if seg:
            temp[i] = seg[1]
    tacobell_sep_score[j][:] = temp



find2 = chipotle_reviews
# chipotle_reviews_sub = [0 for r in find2]
chipotle_reviews_bid = [r['business_id'] for r in find2]
chipotle_reviews_date = [r['date'] for r in find2]
chipotle_reviews_star = [r['stars'] for r in find2]
chipotle_sep_score = [[None for asp in Aspects] for r in find2]

for j in range(len(find2)):
    temp = [None for asp in Aspects]
    for i in range(len(Aspects)):
        seg = get_segment(find2[j]['text'], [Aspects[i]], default)
        if seg:
            temp[i] = seg[1]
    chipotle_sep_score[j][:] = temp

tacobell_sep_score = pd.DataFrame(data=tacobell_sep_score)
taco_info = pd.DataFrame(data={'business_id': tacobell_reviews_bid, 'date': tacobell_reviews_date,
                               'stars': tacobell_reviews_star})
taco_score_chart = pd.concat([taco_info, tacobell_sep_score], axis=1)
taco_score_chart.columns = ['business_id', 'date', 'stars']+Aspects


chipotle_sep_score = pd.DataFrame(data=chipotle_sep_score)
chip_info = pd.DataFrame(data={'business_id': chipotle_reviews_bid, 'date': chipotle_reviews_date,
                               'stars': chipotle_reviews_star})
chip_score_chart = pd.concat([chip_info,chipotle_sep_score], axis=1)
chip_score_chart.columns = ['business_id', 'date', 'stars']+Aspects

taco_score_chart.to_csv("taco_amb_sep_score.csv", index=False, sep=',')
chip_score_chart.to_csv("chip_amb_sep_score.csv", index=False, sep=',')
