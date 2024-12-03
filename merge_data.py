import csv
from pprint import pp

header = []
rows_en = []
rows_it = []

uni_map = {}


def map_unis(uni):
    uni = uni.strip().lower()

    if "bocconi" in uni:
        uni = "bocconi"
    elif "bicocca" in uni:
        uni = "bicocca"
    elif "statale" in uni or "studi di milano" in uni:
        uni = "unimi"
    elif "politecnico" in uni and "milano" in uni:
        uni = "polimi"
    elif "politecnico" in uni and "torino" in uni:
        uni = "polito"
    elif "upo" in uni:
        uni = "upo"
    elif "cattolica" in uni:
        uni = "cattolica"
    elif "piemonte orientale" in uni:
        uni = "piemonte orientale"
    elif "brera" in uni:
        uni = "brera"

    if uni in uni_map:
        uni_map[uni] += 1
    else:
        uni_map[uni] = 1

    return uni


with open("data/Stats Research Project (Risposte) - English form.csv") as csv_file_en:
    reader = csv.reader(csv_file_en)
    header = next(reader)

    for data in reader:
        row = []
        data[0]  # time
        row.append(map_unis(data[1]))  # university
        row.append(int(data[2].strip().split(" ")[0]))  # age

        commuter = 1 if data[3] == "Yes" else 0
        row.append(commuter)

        timeScore = (
            1
            if data[4] == "Less than 10 minutes"
            else 2
            if data[4] == "Between 10 and 30 minutes"
            else 3
            if data[4] == "Between 30 minutes and 1 hour"
            else 4
            if data[4] == "Between 1 hour and 1.30 hours"
            else 5
        )
        row.append((timeScore - 1) / 4)

        attendanceScore = -1
        match data[5]:
            case "More than 75%":
                attendanceScore = 4
            case "Between 50% and 75%":
                attendanceScore = 3
            case "Between 25% and 50%":
                attendanceScore = 2
            case "Less than 25%":
                attendanceScore = 1
            case _:
                raise Exception("attendance", data[5])

        row.append((attendanceScore - 1) / 3)

        try:
            gpa = float(data[6].strip().split("/")[0].replace(",", "."))
            row.append((gpa - 1) / 30)
        except Exception:
            row.append(-1)

        # Part 2 - non commuters
        moved = 1 if data[7] == "Yes" else 0
        row.append(moved)

        # Part 3 - commuters
        meanOfTransport = data[8].split(
            ", "
        )  # TODO: test this because for now we only have non-commuters
        row.append(",".join(meanOfTransport))

        thinkCommutingInfluences = int(data[9] or "-8")
        row.append((thinkCommutingInfluences - 1) / 9)

        # TODO: make sure that 1 = good and 0 = bad
        study = int(data[10])
        row.append((study - 1) / 9)

        higherGPA = int(data[10])
        row.append((higherGPA - 1) / 9)

        # Part 4
        hobbies = int(data[10])
        row.append((hobbies - 1) / 9)

        stress = int(data[10])
        row.append((stress - 1) / 9)

        sleep = int(data[10])
        row.append((sleep - 1) / 9)

        friends = int(data[10])
        row.append((friends - 1) / 9)

        family = int(data[10])
        row.append((family - 1) / 9)

        loneliness = int(data[10])
        row.append((loneliness - 1) / 9)

        for i, v in enumerate(row):
            if type(row[i]) is float:
                row[i] = round(row[i], ndigits=2)
        rows_en.append(row)

with open("data/Stats Research Project (Risposte) - Italian form.csv") as csv_file_it:
    reader = csv.reader(csv_file_it)
    next(reader)

    for data in reader:
        row = []
        data[0]  # time
        row.append(map_unis(data[1]))  # university
        row.append(int(data[2].strip().split(" ")[0]))  # age

        commuter = 1 if data[3] == "Si" else 0
        row.append(commuter)

        timeScore = (
            1
            if data[4] == "Meno di 10 minuti"
            else 2
            if data[4] == "Dai 10 ai 30 minuti"
            else 3
            if data[4] == "Da 30 minuti a 1 ora"
            else 4
            if data[4] == "Da 1 ora a 1 ora e mezza"
            else 5
        )
        row.append((timeScore - 1) / 4)

        attendanceScore = -1
        match data[5]:
            case "Più del 75%":
                attendanceScore = 4
            case "Dal 50% al 75%":
                attendanceScore = 3
            case "Dal 25% al 50%":
                attendanceScore = 2
            case "Meno del 25%":
                attendanceScore = 1
            case _:
                raise Exception("attendance", data[5])

        row.append((attendanceScore - 1) / 3)

        try:
            gpa = float(data[6].strip().split("/")[0].replace(",", "."))
            row.append((gpa - 1) / 30)
        except Exception:
            row.append(-1)

        # Part 2 - non commuters
        moved = 1 if data[7] == "Si" else 0
        row.append(moved)

        # Part 3 - commuters
        meanOfTransport = data[8].split(
            ", "
        )  # TODO: test this because for now we only have non-commuters
        row.append(",".join(meanOfTransport))

        thinkCommutingInfluences = int(data[9] or "-8")
        row.append((thinkCommutingInfluences - 1) / 9)

        # TODO: make sure that 1 = good and 0 = bad
        study = int(data[10])
        row.append((study - 1) / 9)

        higherGPA = int(data[10])
        row.append((higherGPA - 1) / 9)

        # Part 4
        hobbies = int(data[10])
        row.append((hobbies - 1) / 9)

        stress = int(data[10])
        row.append((stress - 1) / 9)

        sleep = int(data[10])
        row.append((sleep - 1) / 9)

        friends = int(data[10])
        row.append((friends - 1) / 9)

        family = int(data[10])
        row.append((family - 1) / 9)

        loneliness = int(data[10])
        row.append((loneliness - 1) / 9)

        for i, v in enumerate(row):
            if type(row[i]) is float:
                row[i] = round(row[i], ndigits=2)
        rows_it.append(row)


with open("data/merged.csv", mode="w+") as result:
    writer = csv.writer(result)
    header = [
        "time",
        "university",
        "age",
        "is_commuter",
        "commute_time",
        "attendance",
        "gpa",
        "did_move",
        "means_of_transport",
        "think_commuting_negative_on_studies",
        "no_study_time",
        "higher_gpa_if_closer",
        "no_hobbies",
        "stress",
        "no_sleep",
        "no_friends",
        "no_family",
        "loneliness",
    ]
    writer.writerow(header[1:])
    writer.writerows(rows_en)
    writer.writerows(rows_it)


pp(uni_map)
