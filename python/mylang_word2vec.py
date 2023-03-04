import matplotlib.pyplot as plt
import gensim
from gensim.models import Word2Vec
from gensim.corpora import Dictionary

f = open("../corpus.txt")
datalist = f.readlines()
paras = []
for line in datalist:
    paras.append(line.strip().split())
f.close()

dct = Dictionary(paras)
print(len(dct.dfs.items()))
# word_count = list(reversed(sorted(dct.dfs.values())))
# plt.plot(list(range(1, len(word_count) + 1)), word_count)
# plt.show()

model = Word2Vec(sentences=paras, vector_size=50, window=5, min_count=1)

# print(model.wv.most_similar('n203', topn=200))
print(model.wv['n203'])

words = model.wv.index_to_key
print(words)

x = []
y = []
for i in range(100):
    word = model.wv[words[i]]
    x.append(word[2])
    y.append(word[3])

plt.scatter(x=x, y=y)
plt.show()
