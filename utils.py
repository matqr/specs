from trueskill import Rating, rate, rate_1vs1
from scipy.stats import ttest_ind
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt



def calculate_trait(normal_score, reversed_score):
    return (normal_score + 6 - reversed_score) / 2


def calculate_big5(df):
    """
    Calculate the big5 personality scores based on Big Five Inventory-10 (BFI-10).

    Parameters
    ----------
    df : DataFrame
        Dataframe with a unique respondent ID and 10 columns from BFI-10
        with numerical values

    Returns
    -------
    DataFrame
        A copy of the input parameter with the calculated 5 personality scores

    TypeError: If `df` is not a DataFrame.
    """
    if not isinstance(df, pd.DataFrame):
        raise TypeError("Argument must be a DataFrame")

    trait_df = pd.DataFrame()
    b5p_traits = {
        'extraversion': ('D11r6', 'D11r1'),  # 6, 1 reversed
        'agreeableness': ('D11r2', 'D11r7'),  # 2, 7 reversed
        'conscientiousness': ('D11r8', 'D11r3'),  # 8, 3 reversed
        'neuroticism': ('D11r9', 'D11r4'),  # 9, 4 reversed
        'openness': ('D11r10', 'D11r5')  # 10, 5 reversed
    }

    # compute the trait values
    for trait, (n_score, r_score) in b5p_traits.items():
        trait_df[trait] = calculate_trait(df[n_score], df[r_score])

    # compute where participants are 
    for trait in b5p_traits.keys():
        # statistics per trait
        median_val = trait_df[trait].median()
        q3 = trait_df[trait].quantile(0.75)        
        q1 = trait_df[trait].quantile(0.25)

        # median
        list_new_trait = [1 if trait_val >=
                          median_val else 0 for trait_val in trait_df[trait]]
        trait_df[f'{trait}_median'] = list_new_trait
        
        # q3 and q1
        trait_df[f'{trait}_q3_q1'] = trait_df[trait].apply(
            lambda x: 1 if x >= q3 else (0 if x <= q1 else None)
        )

    # Split the original DataFrame into two parts: before and after the insertion point
    insert_idx = df.columns.get_loc('D11r1')
    df_before = df.iloc[:, :insert_idx]
    df_after = df.iloc[:, insert_idx:]

    # Concatenate the DataFrames: before + new + after
    df_result = pd.concat([df_before, trait_df, df_after], axis=1)

    return df_result


def calculate_trueskill(df, scaling=False, normal_dist=True):
    """
    Compute trueskill scores for pair-wise comparisons in a dataframe

    Parameters
    ----------
    df : DataFrame
        The different pair-wise (or 1 vs 1) comparisons on each row under
        given value of the column `Question`
    scaling : Boolean
        Flag to scale the final values into the range [0,10] to match
        pre-trained models on PP2
    normal-dist Boolean
        Flag to compare games against a dummy normal distribution

    Returns
    -------
    DataFrame
        A n x 4 Dataframe with n unique images and 3 columns (id, question,
        score, and num. of comparisons)
    """
    # dictionary for all indicators, each image is a player
    all_ratings = {}  # {indicator: {player: ratings} }
    all_counts = {}  # {indicator: {player: counts} }

    indicators = df['Question'].unique()

    # default ratings for each new player
    default_rating = Rating()
    default_count = 0

    # placeholder for final list and dataframe
    trueskills_scores_list = []

    for ind in indicators:
        df_ind = df[df['Question'] == ind]

        # initialize a dictionary to hold each player's TrueSkill rating and
        # number of comparisons
        ratings = {}  # {player: ratings}
        counts = {}  # {player: counts}

        for _, row in df_ind.iterrows():
            player1 = row['Left_image']
            player2 = row['Right_image']
            result = row['Score']

            # get the value based on player key, if not key doesn't exist give
            # default values
            rating1 = ratings.get(player1, default_rating)
            rating2 = ratings.get(player2, default_rating)
            count1 = counts.get(player1, default_count)
            count2 = counts.get(player2, default_count)

            # Update ratings based on the result of the match
            if result == 'left':
                # Player1 wins
                if normal_dist:
                    rating1, _ = rate_1vs1(rating1, Rating())
                else:
                    rating1, rating2 = rate_1vs1(rating1, rating2)
            elif result == 'right':
                # Player2 wins
                if normal_dist:
                    rating2, _ = rate_1vs1(rating2, Rating())
                else:
                    rating2, rating1 = rate_1vs1(rating2, rating1)
            elif result == 'equal':
                # Draw
                if normal_dist:
                    (rating1, _), (_, rating2) = rate(
                        [(rating1, Rating()), (Rating(), rating2)], ranks=[0, 0])
                else:
                    (rating1,), (rating2,) = rate(
                        [(rating1,), (rating2,)], ranks=[0, 0])

            # update variables for next games
            counts[player1] = count1 + 1
            counts[player2] = count2 + 1
            ratings[player1] = rating1
            ratings[player2] = rating2

            # end for loop for rows

        all_ratings[ind] = ratings
        all_counts[ind] = counts

        # start building the final list
        for player, rating in ratings.items():
            trueskills_scores_list.append({
                'Image': player,
                'Question': ind,
                'Score': rating.mu,
                'Num_comparisons': counts[player]
            })
        # end for loop for indicators

    # transform to dataframe
    df_trueskills = pd.DataFrame(trueskills_scores_list)

    if scaling:  # scales values [0, 10]
        # theoretical trueskill is [10, 40]
        min_trueskill = 10
        max_trueskill = 40
        df_trueskills['Score'] = 10 * (df_trueskills['Score'] - min_trueskill) / (
            max_trueskill - min_trueskill)

    return df_trueskills


def calculate_trueskill_by_demographic(df, demographic, min_filter=0, scaling=True, normal_dist=False):
    """
    Compute trueskill scores for each value of a demographic `Question`

    Parameters
    ----------
    df : DataFrame
        The different pair-wise (or 1 vs 1) comparisons on each row under
        given value of the column `Question`
    demographic : String
        Column name of the demographic question to slice the DataFrame
    min_filter: int
        Minimum num. of comparisons per image to use as threshold.
        The default value of 0 means will use all records
    scaling : Boolean
        Flag to scale the final values into the range [0,10] to match
        pre-trained models on PP2
    normal-dist Boolean
        Flag to compare games against a dummy normal distribution

    Returns
    -------
    dictionary
        A dictionary of {question_value: DataFrame} where question_value is the
        unique value of the feature question and DataFrame is the result of
        calculating the trueskill scores for the filtered DataFrame
    """
    # placeholder variable
    trueskill_scores_df = {}

    # for each value of the demographic question, calculate trueskill scores
    for d in df[demographic].unique():
        df_filtered = df[df[demographic] == d]

        if min_filter == 0:  # don't filter at all
            trueskill_scores_df[d] = calculate_trueskill(
                df_filtered, scaling=scaling, normal_dist=normal_dist)
        else:  # keep images with at least `min_filter` num. of comparisons
            df_scores = calculate_trueskill(
                df_filtered, scaling=scaling, normal_dist=normal_dist)
            trueskill_scores_df[d] = df_scores[df_scores['Num_comparisons']
                                       >= min_filter]
            
    return trueskill_scores_df


def plot_trueskill_all_countries_by_demographic(
    df,
    demographic,
    min_filter=0,
    scaling=True,
    normal_dist=False,
    print_text=True,
    labels=["Men", "Women"],
    fontsize=14,
    figsize=(16, 10),
):
    """
    Calculates trueskill score values based on demographic groups for a dataframe.
    It also runs student t-test statistic analysis where the null hypothesis is
    there is no significant difference between the means (mu1 = mu2) of the groups.
    Finally plots are generated and saved from this

    """

    indicators = df["Question"].unique()
    num_indicators = len(indicators)

    countries = df["Country"].unique()
    num_countries = len(countries)

    groups_list = df[demographic].unique()

    min_sample_size = 100

    # DEBUG
    print(groups_list)
    print(f"Blue plots are for: {groups_list[0]}")
    print(f"Red plots are for: {groups_list[1]}")

    if min_filter == 0:
        min_filter_overall = 0
    else:
        min_filter_overall = 16
    # all countries
    overall_trueskill_scores_df = calculate_trueskill_by_demographic(
        df, demographic, min_filter=min_filter, scaling=scaling, normal_dist=normal_dist
    )
    # for each country
    countries_trueskills_cores_df = {}
    for c in countries:
        country_df = df[df["Country"] == c]
        countries_trueskills_cores_df[c] = calculate_trueskill_by_demographic(
            country_df, demographic, min_filter=min_filter, scaling=scaling, normal_dist=normal_dist
        )

    # student t-test: plot scores distributions by gender and perform t-test
    sns.set(style="white")
    fontsize = fontsize

    fig, axes = plt.subplots(
        1 + num_countries, num_indicators, figsize=figsize, sharey=True, sharex=True
    )

    # plot aggregated countries for all indicators
    for i, ind in enumerate(indicators):
        # for every indicator get the gender populations
        group1_trueskill_scores_df = overall_trueskill_scores_df[groups_list[0]]
        group1_indicator_scores_df = group1_trueskill_scores_df[
            group1_trueskill_scores_df["Question"] == ind
        ]
        group2_trueskill_scores_df = overall_trueskill_scores_df[groups_list[1]]
        group2_indicator_scores_df = group2_trueskill_scores_df[
            group2_trueskill_scores_df["Question"] == ind
        ]
        print(f'\nQuestion: {ind}')
        print(f'sample sizes of {len(group1_indicator_scores_df)} and {len(group2_indicator_scores_df)}')

        # t-test
        # Null hypothesis: There is no significant difference between the means (mu1 = mu2)
        t_stat, p_value = ttest_ind(
            group1_indicator_scores_df["Score"],
            group2_indicator_scores_df["Score"],
            nan_policy="omit",
        )
        sig = ""
        if p_value < 0.001:
            sig = "***"
        elif p_value < 0.05:
            sig = "**"
        elif p_value < 0.1:
            sig = "*"

        text = (
            # f"T-statistic:\n{t_stat:.4f}\nP-value:\n{p_value:.4f}\n{sig}"
            f"p-value:\n{p_value:.4f}\n{sig}"
            if print_text
            else ""
        )

        ax = axes[0, i]  # Use the shared axes

        # KDE
        sns.kdeplot(
            group1_indicator_scores_df["Score"],
            ax=ax,
            color="blue",
            fill=True,
            label=labels[0],
        )
        sns.kdeplot(
            group2_indicator_scores_df["Score"],
            ax=ax,
            color="red",
            fill=True,
            label=labels[1],
        )

        # t-test results
        ax.text(
            0.05,
            0.95,
            text,
            transform=ax.transAxes,
            fontsize=fontsize - 2,
            verticalalignment="top",
            color="black",
        )

        ax.set_title(f"{ind}", size=fontsize)
        ax.set_xlabel("")
        ax.set_ylabel("All", size=fontsize)

        # Add legend only to the first subplot
        if i == 0:
            ax.legend(loc="lower right")

    # plot each country for all indicators
    for j, c in enumerate(countries):
        demographic_trueskill_scores_df = countries_trueskills_cores_df[c]
        for i, ind in enumerate(indicators):
            # for every indicator get the gender populations
            group1_trueskill_scores_df = demographic_trueskill_scores_df[groups_list[0]]
            group1_indicator_scores_df = group1_trueskill_scores_df[
                group1_trueskill_scores_df["Question"] == ind
            ]
            group2_trueskill_scores_df = demographic_trueskill_scores_df[groups_list[1]]
            group2_indicator_scores_df = group2_trueskill_scores_df[
                group2_trueskill_scores_df["Question"] == ind
            ]

            # keep track of smallest sample size per country per indicator
            if len(group1_indicator_scores_df) > len(group2_indicator_scores_df):
                min_sample_size = len(group2_indicator_scores_df)
            else:
                min_sample_size = len(group1_indicator_scores_df)

            # t-test
            # Null hypothesis: There is no significant difference between the means (mu1 = mu2)
            t_stat, p_value = ttest_ind(
                group1_indicator_scores_df["Score"],
                group2_indicator_scores_df["Score"],
                nan_policy="omit",
            )
            sig = ""
            if p_value < 0.001:
                sig = "***"
            elif p_value < 0.05:
                sig = "**"
            elif p_value < 0.1:
                sig = "*"

            text = f"p-value:\n{p_value:.4f}\n{sig}" if print_text else ""

            ax = axes[j + 1, i]  # Use the shared axes
            # KDE
            sns.kdeplot(
                group1_indicator_scores_df["Score"],
                ax=ax,
                color="blue",
                fill=True,
                label=labels[0],
            )
            sns.kdeplot(
                group2_indicator_scores_df["Score"],
                ax=ax,
                color="red",
                fill=True,
                label=labels[1],
            )

            # t-test results
            ax.text(
                0.05,
                0.95,
                text,
                transform=ax.transAxes,
                fontsize=fontsize - 2,
                verticalalignment="top",
                color="black",
            )

            ax.set_xlabel("")
            ax.set_ylabel(c, size=fontsize)
            # Add legend only to the first subplot
            if i == 0:
                ax.legend(loc="lower right")
            
            # end for loop indicators
            
        # end for loop countries

    print(f'\nSmallest sample size in all countries demographic subgroup: {min_sample_size}')

    plt.savefig(f"img/{demographic}_trueskill_bias.png", dpi=300)
    plt.tight_layout()
    plt.show()


def calculate_qscore(df):
    """
    Compute Strenght of Schedule (SOS) q scores for pair-wise comparisons in a dataframe.
    The formula already bounds the scores in [0, 10]

    Parameters
    ----------
    df : DataFrame
        The different pair-wise (or 1 vs 1) comparisons on each row under
        given value of the column `Question`

    Returns
    -------
    DataFrame
        A n x 4 Dataframe with n unique images and 3 columns (id, question,
        score, and num. of comparisons)
    """
    # placeholder for final list and dataframe
    q_scores_list = []

    indicators = df["Question"].unique()
    all_players = df["Left_image"].unique()

    for ind in indicators:
        df_ind = df[df["Question"] == ind]

        counts_player1_df = df_ind.groupby(
            "Left_image")["Score"].value_counts()
        counts_player2_df = df_ind.groupby(
            "Right_image")["Score"].value_counts()

        # dictionaries to hold information of every image {image: values}
        W_i_u_dict, L_i_u_dict = {}, {}
        n_i_w_dict, n_i_l_dict = {}, {}
        num_comparisons_dict = {}
        # dictionary of images that image was selected over
        compared_w_dict = {}  # {image: [image1, image2, ...]}
        # dictionary of images that image was NOT selected over
        compared_l_dict = {}  # {image: [image1, image2, ...]}

        # Calculate W_i_u, L_i_u for all images/players
        for player in all_players:
            # number of times an image was selected over its paired image
            # player won as player1 (left)
            player1_w = df_ind[
                (df_ind["Left_image"] == player) & (df_ind["Score"] == "left")
            ]
            # player won as player2 (right)
            player2_w = df_ind[
                (df_ind["Right_image"] == player) & (
                    df_ind["Score"] == "right")
            ]
            # swap columns to have player only as the Left image
            aux = player2_w["Left_image"]
            player2_w.loc[:, "Left_image"] = player2_w.loc[:, "Right_image"]
            player2_w.loc[:, "Right_image"] = aux
            # stack dataframes into one
            player_w = pd.concat([player1_w, player2_w], ignore_index=True)
            w = len(player_w)
            n_i_w_df = player_w.drop_duplicates(
                subset=["Left_image", "Right_image"])
            n_i_w_dict[player] = len(n_i_w_df)

            # number of times an image was NOT selected over its paired image
            # player lost as player1 (left)
            player1_l = df_ind[
                (df_ind["Left_image"] == player) & (df_ind["Score"] == "right")
            ]
            # player lost as player2 (right)
            player2_l = df_ind[
                (df_ind["Right_image"] == player) & (df_ind["Score"] == "left")
            ]
            # swap columns to have player only as the Left image
            aux = player2_l["Left_image"]
            player2_l.loc[:, "Left_image"] = player2_l.loc[:, "Right_image"]
            player2_l.loc[:, "Right_image"] = aux

            # stack dataframes into a single one
            player_l = pd.concat([player1_l, player2_l], ignore_index=True)
            l = len(player_l)
            n_i_l_df = player_l.drop_duplicates(
                subset=["Left_image", "Right_image"])
            n_i_l_dict[player] = len(n_i_l_df)

            # number of times when image was chosen as equal
            t1 = counts_player1_df.get(player, {}).get("equal", 0)
            t2 = counts_player2_df.get(player, {}).get("equal", 0)
            total = w + l + t1 + t2  # same as num_comparisons

            # W and L rate
            if total != 0:
                W_i_u_dict[player] = w / total
                L_i_u_dict[player] = l / total
            else:
                W_i_u_dict[player], L_i_u_dict[player] = 0, 0

            num_comparisons_dict[player] = total
            compared_w_dict[player] = n_i_w_df["Right_image"].unique()
            compared_l_dict[player] = n_i_l_df["Right_image"].unique()
            # end loop for players

        # Calculate W_j_u_sum, L_j_u_sum
        for player in all_players:
            # sum the win ratio of images that player was selected over
            W_j_u_sum = sum(
                w_i_u
                for win_player, w_i_u in W_i_u_dict.items()
                if win_player in compared_w_dict[player]
            )
            # sum the win ratio of images that player was NOT selected over
            L_j_u_sum = sum(
                l_i_u
                for lose_player, l_i_u in L_i_u_dict.items()
                if lose_player in compared_l_dict[player]
            )

            # Calculate Q score
            q_score = (10 / 3) * (
                W_i_u_dict[player]
                + (1 / (n_i_w_dict[player] or 0.001)) *
                W_j_u_sum  # in case the number is 0
                - (1 / (n_i_l_dict[player] or 0.001)) * \
                L_j_u_sum  # in case the number is 0
                + 1
            )

            # build the final dataframe
            q_scores_list.append(
                {
                    "Image": player,
                    "Question": ind,
                    "Score": q_score,
                    "Num_comparisons": num_comparisons_dict[player],
                }
            )

        # end loop for indicators
    df_q_scores = pd.DataFrame(q_scores_list)

    return df_q_scores


def calculate_qscore_by_demographic(df, demographic, min_filter=0):
    """
    Compute Strenght of Schedule (SOS) q scores for each value of
    a demographic `Question`

    Parameters
    ----------
    df : DataFrame
        The different pair-wise (or 1 vs 1) comparisons on each row under
        given value of the column `Question`
    demographic : String
        Column name of the demographic question to slice the DataFrame
    min_filter: int
        Minimum num. of comparisons per image to use as threshold.
        The default value of 0 means will use all records

    Returns
    -------
    dictionary
        A dictionary of {question_value: DataFrame} where question_value is the
        unique value of the feature question and DataFrame is the result of
        calculating the trueskill scores for the filtered DataFrame
    """

    # placeholder variable
    q_scores_df = {}

    # for each value of the demographic question, calculate trueskill scores
    for d in df[demographic].unique():
        df_filtered = df[df[demographic] == d]

        if min_filter == 0:  # don't filter at all
            q_scores_df[d] = calculate_qscore(df_filtered)
        else:  # keep images with at least `min_filter` num. of comparisons
            df_scores = calculate_qscore(df_filtered)
            q_scores_df[d] = df_scores[df_scores['Num_comparisons']
                                       >= min_filter]

    return q_scores_df
