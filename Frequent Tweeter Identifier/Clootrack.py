# Clootrack Assessment

import collections as col
import operator

useful_list = []


def tweet_input(N_test_cases):
   """The function takes N_test_cases i.e no. of test cases as a natural-number input. Based on that input, it asks for no. of tweets per user in each test case round, thereby calculating the tweet count per user instantly. After all test case rounds are complete and tweet counts are calculated, a final list is made with alphabetic sorting (if necessary) and concatenating all the count results in the final stage."""
   n1=int(N_test_cases)
   if n1>0:
        print('No. of test cases: ', n1)
        for i in range(0, n1, 1):
            print('Test Case ', i + 1, '\n')
            y = input('Please enter no. of tweets: ')
            n2 = int(y)
            tw_list = []
            for j in range(0, n2, 1):
                t = input('Enter username and tweet_id #' + str((j + 1)) + ': ')
                tw_list.append(t)

                def usernames(LIST):
                    un_list = []
                    for i in LIST:
                        un_list.append(i.split(" ")[0])
                    return un_list

                un_list = usernames(tw_list)

                def dict_creator(LIST):
                    oc_cnt = col.Counter(LIST)
                    dict_oc_cnt = dict(oc_cnt)
                    tw_sorted_doc = dict(sorted(dict_oc_cnt.items(), key=operator.itemgetter(1), reverse=True))
                    return tw_sorted_doc

                tw_sorted_doc = dict_creator(un_list)

            M = max(tw_sorted_doc.values())
            w = [k for k, v in tw_sorted_doc.items() if v == M]
            for q in sorted(w):
                useful_list.append(q + ' ' + str(M))

        for i in range(len(useful_list)):
            print(useful_list[i].replace('' '', ""))
   else:
        print(n1,'entered.'+' Please enter a non-zero positive number')

# Please uncomment the following line along with N_test_cases as input to run the program
# tweet_input()


