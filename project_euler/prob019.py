months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

def is_leap_year(year):
    if (year % 4 == 0 and year % 100 != 0) or year % 400 == 0:
        return True
    return False

cur_day = cur_month = sundays = 0
min_year = 1900
max_year = 2000

for i in range(min_year, max_year + 1):
    cur_year = i
    if is_leap_year(cur_year):
        months[1] = 29
    else:
        months[1] = 28
    cur_month = 0
    for j in range(0, sum(months)):
        cur_day += 1
        if j == 0 and cur_day % 7 == 0 and cur_year != min_year:
            sundays += 1
        elif j >= sum(months[:cur_month + 1]):
            cur_month += 1
            if cur_day % 7 == 0 and cur_year != min_year:
                sundays += 1

print sundays