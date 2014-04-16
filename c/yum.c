/*
 * YUM
 *
 * Jeff Tranter tranter AT pobox.com
 *
 * Written in standard C but designed to run on the Apple Replica 1
 * using the CC65 6502 assembler.
 *
 * Copyright 2012-2014 Jeff Tranter
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License four the specific language governing permissions and
 * limitations under the License.
 *
 * TODO:
 * - optimize code size (e.g. use char instead of int)
 * - make computer player smarter
 *
 * Revision History:
 *
 * Version  Date         Comments
 * -------  ----         --------
 * 0.0      25 Jul 2012  Started coding
 * 0.1      31 Jul 2012  First release.
 *
 */

#include <stdio.h>

/*

YUM - a variation of the game Yahtzee

Up to 3 players (set at compile time, limited by screen size).
Players can be the computer.

*/

/* CONSTANTS */

/* Sentinel value indicating that a category has not been played yet. */
#define UNSET -1

/* Maximum number of players */
#define MAXPLAYERS 3

/* Number of categories in score. */
#define MAXCATEGORY 15

/* Number of dice. */
#define MAXDICE 5

/* Number of rounds. */
#define MAXROUNDS 12

/* Number of dice rolls. */
#define MAXROLLS 3

/* Size of buffer variable. */
#define SIZEOFBUFFER 40

/* VARIABLES */

/* Number of human players. */
int numHumanPlayers;

/* Number of computer players. */
int numComputerPlayers;

/* Current round. */
int currentRound;

/* Current player. */
int player;

/* Current dice roll number (1-3). */
int roll;

/* Seed value for random numbers. */
int randomSeed;

/* Returns 1 if a given player is a computer. */
int isComputerPlayer[MAXPLAYERS];

/* Names of each player. */
char playerName[MAXPLAYERS][10];

/* 2D array of values given player number and category. */
int scoreSheet[MAXPLAYERS][MAXCATEGORY];

/* Current dice for player. */
int dice[MAXDICE];

/* General purpose buffer for user input. */
char buffer[40];

/* Names of computer players. */
char *computerNames[MAXPLAYERS];

/* Names of categories. */
char *labels[MAXCATEGORY];

/* FUNCTIONS */

/* Mark all dice to be rolled. */
void markAllDiceToBeRolled()
{
    int p;

    for (p = 0; p < MAXDICE; p++) {
        dice[p] = UNSET;
    }
}

/*
 * Initialize score table to initial values for start of a game. Don't
 * clear player names since we may be starting a new game with the
 * same players.
 */
void initialize()
{
    int p, c;

    for (p = 0; p < MAXPLAYERS; p++) {
        for (c = 0; c < MAXCATEGORY; c++) {
            scoreSheet[p][c] = UNSET;
        }
    }

    /* Initialize all dice to UNSET state too. */
    markAllDiceToBeRolled();

    /* Initialize arrays */
    initptr(computerNames, "Altair", "8080", "Z80", NULL);

    initptr(labels, "1'S", "2'S", "3'S", "4'S", "5'S", "6'S", "Sub-total", "Bonus", "Low Straight", "High Straight", "Low Score", "High Score", "Full House", "Yum", "Total", NULL);
}

/* Clear the screen. */
void clearScreen()
{
    printf("\033[2J\033[H"); /* VT100/ANSI clear screen, cursor home */
}

/* Print a string, wait for user to press enter, then continue. */
void pressEnter(s)
char *s;
{

    /* Default string is printed if s is 0. */
    if (s == 0) {
        printf("\nPress <Enter> to continue ");
    } else {
        printf("%s", s);
    }

#ifdef __CC65__
    /* On CC65 platform use keyPressed() routine and use this to set the random seed. */

    while (!keypressed()) {
        randomSeed++;
    }
    readkey();
    printf("\n");
#else
    fgets(buffer, SIZEOFBUFFER-1, stdin);
#endif
}

/* Print a score value as a number. If set to UNSET, display blanks. */
void printField(i)
int i;
{
    if (i == UNSET) {
        printf("        ");
    } else {
        printf("%-8d", i);
    }
}

/* Print a numeric row of the score card. */
void printRow(label, row)
char *label;
int row;
{
    int p;

    printf("%-15s", label);
    for (p = 0; p < numHumanPlayers + numComputerPlayers; p++) {
        printField(scoreSheet[p][row]);
    }
    printf("\n");
}

/* Update scores after a turn, i.e. recalculate totals. */
void updateScore()
{
    int p, i, total;

    for (p = 0; p < numHumanPlayers + numComputerPlayers; p++) {

        /* Calculate sub-total. */
        total = 0;
        for (i = 0; i <= 5 ; i++) {
            if (scoreSheet[p][i] != -1) {
                total += scoreSheet[p][i];
            }
        }
        scoreSheet[p][6] = total;

        /* Calculate bonus. */
        if (scoreSheet[p][6] >= 63) {
            scoreSheet[p][7] = 25;
        } else {
            scoreSheet[p][7] = 0;
        }

        /* Calculate total. */
        total = 0;
        for (i = 6; i <= 13 ; i++) {
            if (scoreSheet[p][i] != -1) {
                total += scoreSheet[p][i];
            }
        }
        scoreSheet[p][14] = total;
    }
}

/* Display the current score card. Displays final results if all categories are played. */
void displayScore()
{
    int i;

    printf("Score after %d of 12 rounds:\n\n", currentRound);
    printf("Roll           ");
    for (i = 0; i < numHumanPlayers + numComputerPlayers; i++) {
        printf("%-8s", playerName[i]);
    }
    printf("\n");

    for (i = 0; i < MAXCATEGORY; i++) {
        printRow(labels[i], i);
    }
}

/* Display help information. */
void displayHelp()
{
    printf("%s", "This is a computer version of the game Yum, similar to games known as Yahtzee,\n");
    printf("%s", "Yacht and Generala. Each player rolls five dice up to three times and then\n");
    printf("%s", "applies the dice toward a category to claim points. The game has 12 rounds\n");
    printf("%s", "during which each player attempts to claim the most points in each category.\n\n");
    printf("%s", "The winner is the person scoring the most points at the end of the game.\n\n");
    printf("%s", "This version supports up to three players of which any can be human or\n");
    printf("%s", "computer players.\n\n");
    printf("%s", "Categories are as follows:\n");
    printf("%s", "1'S through 6'S - dice of same type\n");
    printf("%s", "Low Straight (15) - 1 2 3 4 5\n");
    printf("%s", "High Straight (20) - 2 3 4 5 6\n");
    printf("%s", "Low Score - total 21 or more\n");
    printf("%s", "High Score - total 22 or more\n");
    printf("%s", "Full House (25) - 3 of a kind and pair\n");
    printf("%s", "Yum (30) - 5 dice the same\n");
    printf("%s", "Bonus of 25 points if upper section is 63 or more.\n\n");
}

/* Return location of number i in dice array. */
int find(die)
int die;
{
    int i;

    for (i = 0; i < MAXDICE; i++) {
        if (dice[i] == die)
            return i;
    }
    return UNSET;
}

/* Return number of dice that are n. */
int numberOf(n)
int n;
{
    int i, sum;

    sum = 0;

    for (i = 0; i < MAXDICE; i++) {
        if (dice[i] == n) {
            sum += 1;
        }
    }
    return sum;
}

/*
 *  Return if dice form a low straight.
 * The dice must be sorted in order from low to high.
 */
int haveLowStraight()
{
    int i;

    for (i = 0; i < MAXDICE; i++) {
        if (dice[i] != i + 1) {
            return 0;
        }
    }
    return 1;
}

/*
 *  Return if dice form a high straight.
 * The dice must be sorted in order from low to high.
 */
int haveHighStraight()
{
    int i;

    for (i = 0; i < MAXDICE; i++) {
        if (dice[i] != i + 2) {
            return 0;
        }
    }
    return 1;
}

/* Return sum of dice. */
int sum()
{
    int i, sum;

    sum = 0;

    for (i = 0; i < MAXDICE; i++) {
            sum += dice[i];
    }
    return sum;
}

/* Return if dice contains number i. */
int contains(die)
int die;
{
    int i;

    for (i = 0; i < MAXDICE; i++) {
        if (dice[i] == die)
            return 1;
    }
    return 0;
}

/* Return if dice form a full house. Dice must be sorted in order. */
int haveFullHouse()
{
    if ((dice[0] == dice[1]) && (dice[1] == dice[2]) && (dice[3] == dice[4]))
        return 1;
    if ((dice[0] == dice[1]) && (dice[2] == dice[3]) && (dice[3] == dice[4]))
        return 1;
    return 0;
}

/* Return if dice form a Yum. */
int haveYum()
{
    int i;

    for (i = 1; i < MAXDICE; i++) {
        if (dice[i] != dice[0]) {
            return 0;
        }
    }
    return 1;
}

/* Determine if we have four the same. If so, return the number we have and set d to the dice that is wrong. */
int haveFourTheSame(d)
int *d;
{
    int i, j;

    for (i = 1; i <= 6; i++) {
        if (numberOf(i) == 4) {
            /* Found 4 the same. */
            for (j = 0; j < MAXDICE; j++) {
                /* Find the one that doesn't match. */
                if (dice[j] != i) {
                    *d = j;
                    return i;
                }
            }
        }
    }

    /* We don't have 4 the same. */
    return 0;
}

/* Determine if we have three the same. If so, return the number we have. */
int haveThreeTheSame()
{
    int i;

    for (i = 1; i <= 6; i++) {
        if (numberOf(i) == 3) {
            return i;
        }
    }

    /* We don't have 3 the same. */
    return 0;
}
/* Determine if we have two the same. If so, return the number we have. */
int haveTwoTheSame()
{
    int i;

    for (i = 1; i <= 6; i++) {
        if (numberOf(i) == 2) {
            return i;
        }
    }

    /* We don't have 2 the same. */
    return 0;
}

/*
 * Determine if we almost have a full house. Returns 1 if so, and
 * sets d to the index of dice that is wrong. e.g. for 22335 would
 * return 1 and 4.
 */
int possFullHouse(d)
int *d;
{
    /* Three possibilities: ijjkk iijkk iijjk */

    if ((dice[1] == dice[2]) && (dice[3] == dice[4])) {
        *d = 0;
        return 1;
    } else if ((dice[0] == dice[1]) && (dice[3] == dice[4])) {
        *d = 2;
        return 1;
    } else if ((dice[0] == dice[1]) && (dice[2] == dice[3])) {
        *d = 4;
        return 1;
    }

    return 0;
}

/*
 * Determine if we almost have a high straight. Returns 1 if so, and
 * sets d to the index of dice that is wrong. e.g. for 23356 would
 * return 1 and 2.
 */
int possHighStraight(d)
int *d;
{
    int i, count;

    count = 0;

    /* See if each value from 2 to 6 appears. */
    for (i = 2; i <= 6; i++) {
        if (contains(i)) {
            count += 1;
        }
    }

    if (count == 4) {
        /* We have a possible low straight. Now which dice is wrong? Either one that occurs twice or is a 1. */
        for (i = 0; i < MAXDICE; i++) {
            if ((dice[i] == 1) || (numberOf(dice[i]) == 2)) {
                *d = i;
                return 1;
            }
        }
    }
    return 0;
}

/* Same as above but for low straight, i.e. 12345 */
int possLowStraight(d)
int *d;
{
    int i, count;

    count = 0;

    /* See if each value from 1 to 5 appears. */
    for (i = 1; i <= 5; i++) {
        if (contains(i)) {
            count += 1;
        }
    }

    if (count == 4) {
        /* We have a possible low straight. Now which dice is wrong? Either one that occurs twice or is a 6. */
        for (i = 0; i < MAXDICE; i++) {
            if ((dice[i] == 6) || (numberOf(dice[i]) == 2)) {
                *d = i;
                return 1;
            }
        }
    }
    return 0;
}

/* Return if we have three of a kind. If so, set the dice that are not the same to UNSET. */
int handleThreeOfAKind()
{
    int i, kind;

    for (i = 6; i > 0; i--) {
        if (numberOf(i) == 3) {
            kind = i;
            for (i = 0; i < MAXDICE; i++) {
                if (dice[i] != kind) {
                    dice[i] = UNSET;
                }
            }
            return 1;
        }
    }
    return 0;
}

/* Return if we have two of a kind. If so, set the dice that are not the same to UNSET. */
int handleTwoOfAKind()
{
    int i, kind;

    for (i = 6; i > 0; i--) {
        if (numberOf(i) == 2) {
            kind = i;
            for (i = 0; i < MAXDICE; i++) {
                if (dice[i] != kind) {
                    dice[i] = UNSET;
                }
            }
            return 1;
        }
    }
    return 0;
}

/* Keep the single highest die in a category we have not used. If none, return 0. */
int keepHighest()
{
    int i, kind;

    for (i = MAXDICE - 1; i >= 0; i--) {
        if (scoreSheet[player][i] == UNSET) {
            kind = i - 1;
            for (i = 0; i < MAXDICE; i++) {
                if (dice[i] != kind) {
                    dice[i] = UNSET;
                }
            }
            return 1;
        }
    }
    return 0;
}

/* Display dice being kept and being rolled again. */
void rolledAgain()
{
    int i;

    printf("%s keeps:", playerName[player]);

    for (i = 0; i < MAXDICE; i++) {
        if (dice[i] != UNSET)
            printf(" %d", dice[i]);
    }

    printf("\n");
}

/* Display a string and get an input string. */
char *promptString(string)
char *string;
{
    printf("%s? ", string);
    fgets(buffer, SIZEOFBUFFER-1, stdin);
    buffer[strlen(buffer)-1] = '\0'; /* Remove newline at end of string */
    return buffer;
}

/* Display a string and prompt for a number. Number must be in range
 * min through max. Returns numeric value.
 */
int promptNumber(string, min, max)
char *string;
int min;
int max;
{
    int val;
    char *endptr;

    val = 0;
    endptr = 0;

    while (1) {
        printf("%s (%d-%d)? ", string, min, max);
        fgets(buffer, SIZEOFBUFFER-1, stdin);
        val = atoi(buffer);
        if ((val >= min) && (val <= max))
            return val;
    }
}

/*
 * Ask number and names of players. Sets values of numPlayers,
 *  isComputerPlayer, and playerName.
 */
void setPlayers()
{
    int i;

    numHumanPlayers = promptNumber("How many human players", 0, MAXPLAYERS);
    if (numHumanPlayers < MAXPLAYERS) {
        numComputerPlayers = promptNumber("How many computer players", (numHumanPlayers == 0) ? 1 : 0, MAXPLAYERS - numHumanPlayers);
    } else {
        numComputerPlayers = 0;
    }

    for (i = 0; i < numHumanPlayers; i++) {
        sprintf(buffer, "Name of player %d", i+1);
        strcpy(playerName[i], promptString(buffer));
        playerName[i][strlen(playerName[i])] = 0; /* May need to add terminating null */
        isComputerPlayer[i] = 0;
    }

    for (i = numHumanPlayers; i < numHumanPlayers + numComputerPlayers; i++) {
        strcpy(playerName[i], computerNames[i - numHumanPlayers]);
        isComputerPlayer[i] = 1;
    }
}

/* Display a string and prompt for Y or N. Return intean value. */
int promptYesNo(string)
char *string;
{
    while (1) {
        printf("%s (y/n) ? ", string);
        fgets(buffer, SIZEOFBUFFER-1, stdin);
        if (toupper(buffer[0]) == 'Y')
            return 1;
        if (toupper(buffer[0]) == 'N')
            return 0;
    }
}

/* Compare function for sorting. */
int compare(i, j)
void *i;
void *j;
{
    return *i - *j;
}

/* Sort dice. */
void sortDice()
{
    qsort(dice, MAXDICE, sizeof(dice[0]), compare);
}

/* Display what dice user has. */
void displayDice()
{
    int i;

    for (i = 0; i < MAXDICE; i++) {
        printf(" %d", dice[i]);
    }
    printf("\n");
}

/* Ask what what dice to roll again. Return 0 if does not want to roll any. */
int askPlayerDiceToRollAgain()
{
    int i;
    int valid;
    int tempDice[MAXDICE];

    /* Make a copy of the dice in case we have to undo changes after an error. */
    for (i = 0; i < MAXDICE; i++) {
        tempDice[i] = dice[i];
    }

    while (1) {

        printf("Enter dice to roll again or D for dice or S for score: ");
        fgets(buffer, SIZEOFBUFFER-1, stdin);
        buffer[strlen(buffer)-1] = '\0'; /* Remove newline at end of string */

        /* Entering 'S' will display the current score. Useful if you can't remember what categories you used. */
        if (toupper(buffer[0]) == 'S') {
            displayScore();
            continue;
        }

        /* Entering 'D' will display the dice. Useful if it scrolled off the screen. */
        if (toupper(buffer[0]) == 'D') {
            displayDice();
            continue;
        }

        /* If empty string, no dice to roll again and we return with 0 status. */
        if (strlen(buffer) == 0) {
            return 0;
        }

        valid = 1;
        /* First validate the input line. */
        for (i = 0; i < strlen(buffer); i++) {
            /* Validate character. */
            if ((buffer[i] != ' ') && (buffer[i] != ',') && ((buffer[i] < '1') || (buffer[i] > '6'))) {
                printf("Invalid input: '%c', try again.\n", buffer[i]);
                valid = 0;
                break;
            }
        }

        /* Try again if not valid. */
        if (!valid) {
            continue;
        }

        /* Now examine the input line */
        for (i = 0; i < strlen(buffer); i++) {
            /* Skip any spaces or commas */
            if ((buffer[i] == ' ') || (buffer[i] == ',')) {
                continue;
            }

            /* Does it match a die we have? If so, unset it to mark it to be rolled again. */
            if (contains(buffer[i] - '0')) {
                dice[find(buffer[i] - '0')] = UNSET;
            } else {
                printf("You don't have a '%c', try again.\n", buffer[i]);
                valid = 0;
                /* Revert the dice to the original saved values since we may have changed it before the error was detected. */
                for (i = 0; i < MAXDICE; i++) {
                    dice[i] = tempDice[i];
                }
                break;
           }
        }

        /* Try again if not valid. */
        if (!valid) {
            continue;
        }

        return 1;
    }
}

/* Generate random number from low and high inclusive, e.g. randomNumber(1, 6) for a die. Calls rand(). */
int randomNumber(low, high)
int low;
int high;
{
    return rand() % high + low;
}

/* Roll 1 die. */
int rollDie()
{
    return randomNumber(1, 6);
}

/* Roll the dice. Only some dice need to be rolled, the ones that are set to UNSET. */
void rollDice()
{
    int i;

    for (i = 0; i < MAXDICE; i++) {
        if (dice[i] == UNSET) {
            dice[i] = randomNumber(1, 6);
        }
    }
}

/* Play a category. */
void playCategory(category)
int category;
{
    if (scoreSheet[player][category] != UNSET) {
        printf("Internal error: tried to play a category that was already selected!\n");
        return;
    }

    switch (category) {
    case 0: case 1: case 2: case 3: case 4: case 5:
        /* Score is number of the dice times the die value. */
        scoreSheet[player][category] = numberOf(category + 1) * (category + 1);
        break;
    case 8: /* Low straight. */
        scoreSheet[player][category] = haveLowStraight() ? 15 : 0;
        break;
    case 9: /* High straight. */
        scoreSheet[player][category] = haveHighStraight() ? 20 : 0;
        break;
    case 10: /* Low score. Must be 21 or more and less than high score. */
        scoreSheet[player][category] = (sum() >= 21) ? sum() : 0;
        if ((sum() >= 21) && ((scoreSheet[player][10] == UNSET) || (sum() < scoreSheet[player][11]))) {
            scoreSheet[player][category] = sum();
        } else {
            scoreSheet[player][category] = 0;
        }
        break;
    case 11: /* High score. Must be 22 or more and more than low score. */
        if ((sum() >= 22) && (sum() > scoreSheet[player][10])) {
            scoreSheet[player][category] = sum();
        } else {
            scoreSheet[player][category] = 0;
        }
        break;
    case 12: /* Full House. */
        scoreSheet[player][category] = haveFullHouse() ? 25 : 0;
        break;
    case 13: /* YUM. */
        scoreSheet[player][category] = haveYum() ? 30 : 0;
        break;
    }
}

/* Ask what category to claim (only show possible ones). */
int humanPickCategory()
{
    int category;
    char buffer[50];

    printf("\n");

    for (category = 0; category < MAXCATEGORY; category++) {
        /* Some categories need to be skipped. */
        if ((category == 6) || (category == 7) || (category == 14)) {
            continue;
        }

        if (scoreSheet[player][category] == UNSET) {
            printf("%2d  - %s\n", category + 1, labels[category]);
        }
    }

    while (1) {
        sprintf(buffer, "\n%s, What category do you want to claim?", playerName[player]);
        category = promptNumber(buffer, 1, MAXCATEGORY-1) - 1;
        if (scoreSheet[player][category] != UNSET) {
            printf("You already used that category. Try again.\n");
        } else {
            break;
        }
    }

    return category;
}

/* Display winner. */
void displayWinner()
{
    int max;
    int winner;
    int p;

    max = UNSET;
    winner = UNSET;

    for (p = 0; p < numHumanPlayers + numComputerPlayers; p++) {
        if (scoreSheet[p][14] > max) {
            max = scoreSheet[p][14];
            winner = p;
        }
    }

    printf("\n");

    /* Display the winner. */
    if (numHumanPlayers + numComputerPlayers == 3) {
        if ((scoreSheet[0][14] == scoreSheet[1][14]) && (scoreSheet[1][14] == scoreSheet[2][14])) {
            printf("It was a three way tie!\n");
        } else if ((scoreSheet[0][14] == scoreSheet[1][14]) && (scoreSheet[0][14] == max)) {
            printf("It was a tie between %s and %s\n", playerName[0], playerName[1]);
        } else if ((scoreSheet[1][14] == scoreSheet[2][14]) && (scoreSheet[1][14] == max)) {
            printf("It was a tie between %s and %s\n", playerName[1], playerName[2]);
        } else if ((scoreSheet[0][14] == scoreSheet[2][14]) && (scoreSheet[0][14] == max)) {
            printf("It was a tie between %s and %s\n", playerName[0], playerName[2]);
        } else {
            printf("The winner is %s.\n", playerName[winner]);
        }
    }

    if (numHumanPlayers + numComputerPlayers == 2) {
        if (scoreSheet[0][14] == scoreSheet[1][14]) {
            printf("It was a tie between %s and %s\n", playerName[0], playerName[1]);
        } else {
            printf("The winner is %s.\n", playerName[winner]);
        }
    }

    /* Display player's scores. */
    for (p = 0; p < numHumanPlayers + numComputerPlayers; p++) {
        printf("%s has %d points.\n", playerName[p], scoreSheet[p][14]);
    }
}

/*
 * Computer decides what dice to roll again (by setting them to
 * UNSET). Returns 0 if does not want to roll any.
*/
int askComputerDiceToRollAgain()
{
    int n, d;

    /* If we have YUM and have not claimed it, don't roll any. */
    if (haveYum() && scoreSheet[player][13] == UNSET) {
        return 0;
    }

    /* If we have all the same and have not claimed that category don't roll any. */
    if (haveYum() && scoreSheet[player][dice[0] - 1] == UNSET) {
        return 0;
    }

    /* If we have a full house and have not claimed that category don't roll any. */
    if (haveFullHouse() && scoreSheet[player][12] == UNSET) {
        return 0;
    }

    /* If we have a high straight and have not claimed that category don't roll any. */
    if (haveHighStraight() && scoreSheet[player][9] == UNSET) {
        return 0;
    }

    /* If we have a low straight and have not claimed that category don't roll any. */
    if (haveLowStraight() && scoreSheet[player][8] == UNSET) {
        return 0;
    }

    /* If we have 4 the same and have not claimed that category then roll the remaining die. */
    if ((n = haveFourTheSame(&d)) && scoreSheet[player][dice[n - 1]] == UNSET) {
        dice[d] = UNSET;
        return 1;
    }

    /* If we almost have a full house and have not claimed that category then roll the remaining die. */
    if (possFullHouse(&d) && scoreSheet[player][12] == UNSET) {
        dice[d] = UNSET;
        return 1;
    }

    /* If we almost have a high straight and have not claimed that category then roll the remaining die. */
    if (possHighStraight(&d) && scoreSheet[player][9] == UNSET) {
        dice[d] = UNSET;
        return 1;
    }

    /* If we almost have a low straight and have not claimed that category then roll the remaining die. */
    if (possLowStraight(&d) && scoreSheet[player][8] == UNSET) {
        dice[d] = UNSET;
        return 1;
    }

    /* If we have 3 the same, roll the remaining dice. */
    if (handleThreeOfAKind()) {
        return 1;
    }

    /* If we have 2 the same, roll the remaining dice. */
    if (handleTwoOfAKind()) {
        return 1;
    }

    /* Keep the 1 highest dice in a category we have not completed. */
    if (keepHighest()) {
        return 1;
    }

    /* If all else fails, roll them all again */
    markAllDiceToBeRolled();
    return 1;
}

/* Computer decides what category to play. */
int pickCategory()
{
    int n, d;

    /* Try for YUM. */
    if (haveYum() && scoreSheet[player][13] == UNSET) {
        return 13;
    }

    /* Try all the same. */
    if (haveYum() && scoreSheet[player][dice[0] - 1] == UNSET) {
        return dice[0] - 1;
    }

    /* Try full house. */
    if (haveFullHouse() && scoreSheet[player][12] == UNSET) {
        return 12;
    }

    /* Try high straight. */
    if (haveHighStraight() && scoreSheet[player][9] == UNSET) {
        return 9;
    }

    /* Try low straight. */
    if (haveLowStraight() && scoreSheet[player][8] == UNSET) {
        return 8;
    }

    /* Try 4 the same. */
    if ((n = haveFourTheSame(&d)) && scoreSheet[player][n - 1] == UNSET) {
        return n - 1;
    }

    /* Try 3 the same. */
    if ((n = haveThreeTheSame()) && scoreSheet[player][n - 1] == UNSET) {
        return n - 1;
    }

    /* Try high score. Must be 22 or more. */
    if ((sum() >= 22) && (scoreSheet[player][11] == UNSET) && (sum() > scoreSheet[player][10])) {
        return 11;
    }

    /* Try low score. Must be 21 or more. */
    if ((sum() >= 21) && (scoreSheet[player][10] == UNSET) && ((scoreSheet[player][11] == UNSET) || (sum() < scoreSheet[player][11]))) {
        return 10;
    }

    /* Try the highest 2 the same. */
    if ((n = haveTwoTheSame()) && scoreSheet[player][n - 1] == UNSET) {
        return n - 1;
    }

    /* Try the highest 1 the same. */
    for (d = MAXDICE - 1; d >= 0; d--) {
        if (scoreSheet[player][dice[d] - 1] == UNSET) {
            return dice[d] - 1;
        }
    }

    /* Throw away the lowest unused category. */
    for (d = 0; d < MAXCATEGORY; d++) {
        if (scoreSheet[player][d] == UNSET) {
            return d;
        }
    }

    return 0;
}

/* Set random seed for the rand() function. */
void setRandomSeed()
{
#ifdef __CC65__

    /* On embedded CC65 systems like the Replica use time taken for key to be pressed. */
    srand(randomSeed);

#else

    /* On desktop systems use system time as random seed. */
    srand(0);

#endif
}

/* Main program. */
int main()
{
    initialize();
    clearScreen();

    printf("%s",
          "#     # #     # #     #\n #   #  #     # ##   ##\n  # #   #     # # # # #\n   #    #     # #  #  #\n   #    #     # #     #\n   #    #     # #     #\n   #     #####  #     #\n\n\nWelcome to Yum!\n");

    if (promptYesNo("Do you want instructions")) {
        clearScreen();
        displayHelp();
    }

    setPlayers();

    while (1) {

        pressEnter("Press <Enter> to start the game");
        setRandomSeed();

        for (currentRound = 1; currentRound <= MAXROUNDS; currentRound++) {
            for (player = 0; player < numHumanPlayers + numComputerPlayers; player++) {

                clearScreen();
                sprintf(buffer, "%s's turn. Press <Enter> to roll ", playerName[player]);
                pressEnter(buffer);
                markAllDiceToBeRolled();

                for (roll = 1; roll <= MAXROLLS; roll++) {
                    int ret;

                    rollDice();
                    sortDice();
                    if (roll == 1) {
                        printf("First roll is:");
                    } else if (roll == 2) {
                        printf("Second roll is:");
                    } else {
                        printf("Last roll is:");
                    }
                    displayDice();
                    if (roll < 3) {
                        if (isComputerPlayer[player]) {
                            ret = askComputerDiceToRollAgain();
                            rolledAgain();
                        } else {
                            ret = askPlayerDiceToRollAgain();
                        }
                        /* Player wants to roll again? */
                        if (ret == 0) {
                            break;
                        }
                    }
                }

                if (isComputerPlayer[player]) {
                    int c;
                    c = pickCategory();
                    printf("%s plays %s\n", playerName[player], labels[c]);
                    playCategory(c);
                    pressEnter(0);
                } else {
                    playCategory(humanPickCategory());
                }

            }
            clearScreen();
            updateScore();
            displayScore();
            pressEnter(0);
        }

        displayWinner();

        pressEnter(0);
        clearScreen();

        if (!promptYesNo("Would you like to play again")) {
            break;
        }

        if (!promptYesNo("Same players as last time")) {
            setPlayers();
        }

        initialize();
    }
    return 0;
}
