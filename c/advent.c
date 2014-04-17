/*
 * 
 * The Abandoned Farm House Adventure
 *
 * Jeff Tranter <tranter@pobox.com>
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
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Revision History:
 *
 * Version  Date         Comments
 * -------  ----         --------
 * 0.0      13 Mar 2012  First alpha version
 * 0.1      18 Mar 2012  First beta version
 * 0.9      19 Mar 2012  First public release
 *
 */

#include <stdio.h>

/* CONSTANTS */

/* Maximum number of items user can carry */
#define MAXITEMS 5

/* Number of locations */
#define NUMLOCATIONS 32

/* Size of input buffer */
#define SIZEOFBUFFER 40

/* TYPES */

/* Directions */
#define North 0
#define South 1
#define East  2
#define West  3
#define Up    4
#define Down  5

/* Items */
#define NoItem     0
#define Key        1
#define Pitchfork  2
#define Flashlight 3
#define Lamp       4
#define Oil        5
#define Candybar   6
#define Bottle     7
#define Doll       8
#define ToyCar     9
#define Matches    10
#define GoldCoin   11
#define SilverCoin 12
#define StaleMeat  13
#define Book       14
#define Cheese     15
#define OldRadio   16
#define LastItem   16

/* Locations */

#define NoLocation       0
#define Driveway1        1
#define Driveway2        2
#define Driveway3        3
#define Driveway4        4
#define Driveway5        5
#define Garage           6
#define WorkRoom         7
#define Hayloft          8
#define Kitchen          9
#define DiningRoom       10
#define BottomStairs     11
#define DrawingRoom      12
#define Study            13
#define TopStairs        14
#define BoysBedroom      15
#define GirlsBedroom     16
#define MasterBedroom    17
#define ServantsQuarters 18
#define LaundryRoom      19
#define FurnaceRoom      20
#define VacantRoom       21
#define Cistern          22
#define Tunnel           23
#define Woods24          24
#define Woods25          25
#define Woods26          26
#define WolfTree         27
#define Woods28          28
#define Woods29          29
#define Woods30          30
#define Woods31          31

/* TABLES */

/* Names of directions */
char *DescOfDirection[6];

/* Names of items */
char *DescOfItem[LastItem+1];

/* Names of locations */
char *DescOfLocation[NUMLOCATIONS];

/* DATA */

/* Inventory of what player is carrying */
int Inventory[MAXITEMS];

/* Location of each item. Index is the item number, returns the location. 0 if item is gone */
int locationOfItem[LastItem+1];

/* Map. Given a location and a direction to move, returns the location it connects to, or 0 if not a valid move. Map can change during game play. */
int Move[NUMLOCATIONS][6];

/* Current location */
int currentLocation;

/* Number of turns played in game */
int turnsPlayed;

/* True if player has lit the lamp. */
int lampLit;

/* True if lamp filled with oil. */
int lampFilled;

/* True if player ate food. */
int ateFood;

/* True if player drank water. */
int drankWater;

/* Incremented each turn you are in the tunnel. */
int ratAttack;

/* Tracks state of wolf attack */
int wolfState;

/* Set when game is over */
int gameOver;

/* Line of user input */
char buffer[40];

/* Clear the screen */
void clearScreen()
{
    printf("\033[2J\033[H"); /* VT100/ANSI clear screen, cursor home */
}

/* Return 1 if carrying an item */
int carryingItem(item)
char *item;
{
    int i;

    for (i = 0; i < MAXITEMS; i++) {
        if ((Inventory[i] != 0) && (!strcmp(DescOfItem[Inventory[i]], item)))
            return 1;
    }
    return 0;
}

/* Return 1 if item is at current location (not carried) */
int itemIsHere(item)
 char *item;
{
    int i;

    /* Find number of the item. */
    for (i = 1; i <= LastItem; i++) {
        if (!strcmp(item, DescOfItem[i])) {
            /* Found it, but is it here? */
            if (locationOfItem[i] == currentLocation) {
                return 1;
            } else {
                return 0;
            }
        }
    }
    return 0;
}

/* Inventory command */
void doInventory()
{
    int i;
    int found;

    found = 0;

    printf("%s", "You are carrying:\n");
    for (i = 0; i < MAXITEMS; i++) {
        if (Inventory[i] != 0) {
            printf("  %s\n", DescOfItem[Inventory[i]]);
            found = 1;
        }
    }
    if (!found)
        printf("  nothing\n");
}

/* Help command */
void doHelp()
{
    printf("%s", "Valid Commands:\n");
    printf("%s", "Go east/west/north/south/up/down\n");
    printf("%s", "Look\n");
    printf("%s", "Use <object>\n");
    printf("%s", "Examine <object>\n");
    printf("%s", "Take <object>\n");
    printf("%s", "Drop <object>\n");
    printf("%s", "Inventory\n");
    printf("%s", "Help\n");
    printf("%s", "Quit\n");
    printf("%s", "You can abbreviate commands and directions to the first letter.\n");
    printf("%s", "Type just the first letter of a direction to move.\n");
}

/* Look command */
void doLook()
{
    int i, loc, seen;

    printf("You are %s.\n", DescOfLocation[currentLocation]);

    seen = 0;
    printf("You see:\n");
    for (i = 1; i <= LastItem; i++) {
        if (locationOfItem[i] == currentLocation) {
            printf("  %s\n", DescOfItem[i]);
            seen = 1;
        }
    }
    if (!seen)
        printf("  nothing special\n");

    printf("You can go:");

    for (i = North; i <= Down; i++) {
        loc = Move[currentLocation][i];
        if (loc != 0) {
            printf(" %s", DescOfDirection[i]);
        }
    }
    printf("\n");
}

/* Quit command */
void doQuit()
{
    printf("%s", "Are you sure you want to quit (y/n)? ");
    fgets(buffer, SIZEOFBUFFER-1, stdin);
    if (tolower(buffer[0]) == 'y') {
        gameOver = 1;
    }
}

/* Drop command */
void doDrop()
{
    int i;
    int sp;
    char *item;

    /* Command line should be like "D[ROP] ITEM" Item name will be after after first space. */
    sp = index(buffer, " ");
    if (sp == -1) {
        printf("Drop what?\n");
        return;
    }

    item = buffer + sp + 1;

    /* See if we have this item */
    for (i = 0; i < MAXITEMS; i++) {
        if ((Inventory[i] != 0) && (!strcmp(DescOfItem[Inventory[i]], item))) {
            /* We have it. Add to location. */
            locationOfItem[Inventory[i]] = currentLocation;
            /* And remove from inventory */
            Inventory[i] = 0;
            printf("Dropped %s.\n", item);
            ++turnsPlayed;
            return;
        }
    }
    /* If here, don't have it. */
    printf("Not carrying %s.\n", item);
}

/* Take command */
void doTake()
{
    int i, j;
    int sp;
    char *item;

    /* Command line should be like "T[AKE] ITEM" Item name will be after after first space. */
    sp = index(buffer, " ");
    if (sp == -1) {
        printf("Take what?\n");
        return;
    }

    item = buffer + sp + 1;

    /* Find number of the item. */
    for (i = 1; i <= LastItem; i++) {
        if (!strcmp(item, DescOfItem[i])) {
            /* Found it, but is it here? */
            if (locationOfItem[i] == currentLocation) {
            /* It is here. Add to inventory. */
            for (j = 0; j < MAXITEMS; j++) {
                if (Inventory[j] == 0) {
                    Inventory[j] = i;
                    /* And remove from location. */
                    locationOfItem[i] = 0;
                    printf("Took %s.\n", item);
                    ++turnsPlayed;
                    return;
                }
            }

            /* Reached maximum number of items to carry */ 
            printf("You can't carry any more. Drop something.\n");
            return;
            }
        }
    }

    /* If here, don't see it. */
    printf("I see no %s here.\n", item);
}

/* Go command */
void doGo()
{
    int  sp;
    char dirChar;
    int  dir;

    /* Command line should be like "G[O] N[ORTH]" Direction will be
       the first letter after a space. Or just a single letter
       direction N S E W U D or full directon NORTH etc. */

    sp = index(buffer, " ");
    if (sp != -1) {
        dirChar = *(buffer + sp + 1);
    } else {
        dirChar = tolower(buffer[0]);
    }

    if (dirChar == 'n') {
        dir = North;
    } else if (dirChar == 's') {
        dir = South;
    } else if (dirChar == 'e') {
        dir = East;
    } else if (dirChar == 'w') {
        dir = West;
    } else if (dirChar == 'u') {
        dir = Up;
    } else if (dirChar == 'd') {
        dir = Down;
    } else {
        printf("Go where?\n");
        return;
    }

    if (Move[currentLocation][dir] == 0) {
        printf("You can't go %s from here.\n", DescOfDirection[dir]);
        return;
    }

    /* We can move */
    currentLocation = Move[currentLocation][dir];
    printf("You are %s.\n", DescOfLocation[currentLocation]);
    ++turnsPlayed;
}

/* Examine command */
void doExamine()
{
    int  sp;
    char *item;

    /* Command line should be like "E[XAMINE] ITEM" Item name will be after after first space. */
    sp = index(buffer, " ");
    if (sp == -1) {
        printf("Examine what?\n");
        return;
    }

    item = buffer + sp + 1;
    ++turnsPlayed;

    /* Examine bookcase - not an object */
    if (!strcmp(item, "bookcase")) {
        printf("You pull back a book and the bookcase opens up to reveal a secret room.\n");
        Move[17][North] = 18;
        return;
    }

    /* Make sure item is being carried or is in the current location */
    if (!carryingItem(item) && !itemIsHere(item)) {
        printf("I don't see it here.\n");
        return;
    }

    /* Examine Book */
    if (!strcmp(item, "book")) {
        printf("It is a very old book entitled \"Altair 8800 Operation Manual\".\n");
        return;
    }

    /* Examine Flashlight */
    if (!strcmp(item, "flashlight")) {
        printf("It doesn't have any batteries.\n");
        return;
    }

    /* Examine toy car */
    if (!strcmp(item, "toy car")) {
        printf("It is a nice toy car. Your grandson Matthew would like it.\n");
        return;
    }

    /* Examine old radio */
    if (!strcmp(item, "old radio")) {
        printf("It is a 1940 Zenith 8-S-563 console with an 8A02 chassis.\nYou'd turn it on but the electricity is off.\n");
        return;
    }

   /* Nothing special about this item */
   printf("You see nothing special about it.\n");
}

/* Use command */
void doUse()
{
    int  sp;
    char *item;

    /* Command line should be like "U[SE] ITEM" Item name will be after after first space. */
    sp = index(buffer, " ");
    if (sp == -1) {
        printf("Use what?\n");
        return;
    }

    item = buffer + sp + 1;

    /* Make sure item is being carried or is in the current location */
    if (!carryingItem(item) && !itemIsHere(item)) {
        printf("I don't see it here.\n");
        return;
    }

    ++turnsPlayed;

    /* Use key */
    if (!strcmp(item, "key") && (currentLocation == VacantRoom)) { /*  */
        printf("You insert the key in the door and it opens, revealing a tunnel.\n");
        Move[21][North] = 23;
        return;
    }

    /* Use pitchfork */
    if (!strcmp(item, "pitchfork") && (currentLocation == WolfTree) && (wolfState == 0)) {
        printf("You jab the wolf with the pitchfork. It howls and runs away.\n");
        wolfState = 1;
        return;
    }

    /* Use toy car */
    if (!strcmp(item, "toy car") && (currentLocation == WolfTree && wolfState == 1)) {
        printf("You show Matthew the toy car and he comes down to take it.\nYou take Matthew in your arms and carry him home.\n");
        wolfState = 2;
        return;
    }

    /* Use oil */
    if (!strcmp(item, "oil")) {
        if (carryingItem("lamp")) {
            printf("You fill the lamp with oil.\n");
            lampFilled = 1;
            return;
        } else {
            printf("You don't have anything to use it with.\n");
            return;
        }
    }

    /* Use matches */
    if (!strcmp(item, "matches")) {
        if (carryingItem("lamp")) {
            if (lampFilled) {
                printf("You light the lamp. You can see!\n");
                lampLit = 1;
                return;
            } else {
                printf("You can't light the lamp. It needs oil.\n");
                return;
            }
        } else {
            printf("Nothing here to light\n");
        }
    }
                
    /* Use candybar */
    if (!strcmp(item, "candybar")) {
        printf("That hit the spot. You no longer feel hungry.\n");
        ateFood = 1;
        return;
    }

    /* Use bottle */
    if (!strcmp(item, "bottle")) {
        if (currentLocation == Cistern) {
            printf("You fill the bottle with water from the cistern and take a drink.\nYou no longer feel thirsty.\n");
            drankWater = 1;
            return;
        } else {
            printf("The bottle is empty. If only you had some water to fill it!\n");
            return;
        }
    }

    /* Use stale meat */
    if (!strcmp(item, "stale meat")) {
        printf("The meat looked and tasted bad. You feel very sick and pass out.\n");
        gameOver = 1;
        return;
    }

    /* Default */
    printf("Nothing happens\n");
}

/* Prompt user and get a line of input */
void prompt()
{
    int i;

    printf("? ");        
    fgets(buffer, SIZEOFBUFFER-1, stdin);

    /* Remove trailing newline */
    buffer[strlen(buffer)-1] = '\0';

    /* Convert buffer to lower case */
    for (i = 0; i < strlen(buffer); i++) 
        buffer[i] = tolower(buffer[i]);
}

/* Do special things unrelated to command typed. */
void doActions()
{
    if ((turnsPlayed == 10) && !lampLit) {
        printf("It will be getting dark soon. You need some kind of light or soon you won't\nbe able to see.\n");
    }

    if ((turnsPlayed >= 60) && (!lampLit || (!itemIsHere("Lamp") && !carryingItem("Lamp")))) {
        printf("It is dark out and you have no light. You stumble around for a while and then\nfall, hit your head, and pass out.\n");
        gameOver = 1;
        return;
    }

    if ((turnsPlayed == 20) && !drankWater) {
        printf("You are getting very thirsty. You need to get a drink soon.\n");
    }

    if ((turnsPlayed == 30) && !ateFood) {
        printf("You are getting very hungry. You need to find something to eat.\n");
    }

    if ((turnsPlayed == 50) && !drankWater) {
        printf("You pass out due to thirst.\n");
        gameOver = 1;
        return;
    }

    if ((turnsPlayed == 40) && !ateFood) {
        printf("You pass out from hunger.\n");
        gameOver = 1;
        return;
    }

    if (currentLocation == Tunnel) {
        if (itemIsHere("cheese")) {
            printf("The rats go after the cheese.\n");
        } else {
            if (ratAttack < 3) {
                printf("The rats are coming towards you!\n");
                ++ratAttack;
            } else {
                printf("The rats attack and you pass out.\n");
                gameOver = 1;
                return;
            }
        }
    }

    /* wolfState values:  0 - wolf attacking 1 - wolf gone, Matthew in tree. 2 - Matthew safe, you won. Game over. */
    if (currentLocation == WolfTree) {
        switch (wolfState) {
            case 0:
                printf("A wolf is circling around the tree. Matthew is up in the tree.\nYou have to save him! If only you had some kind of weapon!\n");
                break;
            case 1:
                printf("Matthew is afraid to come down from the tree. If only you had something to\ncoax him with.\n");
                break;
            case 2:
                printf("Congratulations! You succeeded and won the game. I hope you had as much fun\nplaying the game as i did creating it.\n- Jeff Tranter <tranter@pobox.com>\n");
                gameOver = 1;
                return;
                break;
            }
    }
}

/* Set variables to values for start of game */
void initialize()
{
    currentLocation = Driveway1;
    lampFilled = 0;
    lampLit = 0;
    ateFood = 0;
    drankWater = 0;
    ratAttack = 0;
    wolfState = 0;
    turnsPlayed = 0;
    gameOver= 0;

    /* These doors can get changed during game and may need to be reset O*/
    Move[17][North] = 0;
    Move[21][North] = 0;

    /* Initialize arrays */
    initptr(DescOfDirection, "north", "south", "east", "west", "up", "down", NULL);

    initptr(DescOfItem, "", "key", "pitchfork", "flashlight", "lamp", "oil", "candybar", "bottle", "doll", "toy car", "matches", "gold coin", "silver coin", "stale meat", "book", "cheese", "old radio", NULL);

    initptr(DescOfLocation, "", "in the driveway near your car", "in the driveway", "in front of the garage", "in front of the barn", "at the door to the house", "in the garage", "in the workroom of the barn", "in the hayloft of the barn", "in the kitchen", "in the dining room", "at the bottom of the stairs", "in the drawing room", "in the study", "at the top of the stairs", "in a boy's bedroom", "in a girl's bedroom", "in the master bedroom next to a bookcase", "in the servant's quarters", "in the basement laundry room", "in the furnace room", "in a vacant room next to a locked door", "in the cistern", "in an underground tunnel. There are rats here", "in the woods near a trap door", "in the woods", "in the woods", "in the woods next to a tree", "in the woods", "in the woods", "in the woods", "in the woods", NULL);

    /* Need to do this in two parts because the string exceeds the BDS C 255 character limit. */
    initw(Move, "0,0,0,0,0,0,2,0,0,0,0,0,4,1,3,5,0,0,0,0,6,2,0,0,7,2,0,0,0,0,0,0,2,9,0,0,0,0,0,3,0,0,0,4,0,0,8,0,0,0,0,0,0,7,0,10,5,0,0,19,9,0,0,11,0,0,0,0,10,12,14,0,13,0,11,0,0,0,0,12,0,0,0,0,16,0,15,17,0,11,0,0,0,14,0,0");

    initw(Move[16], "0,14,0,0,0,0,0,0,14,0,0,0,0,0,0,0,0,13,0,0,0,20,9,0,21,0,19,0,0,0,0,20,0,22,0,0,0,0,21,0,0,0,24,21,0,0,0,0,29,23,0,26,0,0,26,0,24,0,0,0,27,25,29,0,0,0,0,26,28,0,0,0,0,29,31,27,0,0,28,24,30,26,0,0,31,0,0,29,0,0,0,30,0,29,0,0");

    /* Set inventory to default */
    setmem(Inventory, sizeof(Inventory[0])*MAXITEMS, 0);
    Inventory[0] = Flashlight;

    /* Put items in their default locations */
    locationOfItem[0]  = 0;                /* NoItem */
    locationOfItem[1]  = Driveway1;        /* Key */
    locationOfItem[2]  = Hayloft;          /* Pitchfork */
    locationOfItem[3]  = 0;                /* Flashlight */
    locationOfItem[4]  = WorkRoom;         /* Lamp */
    locationOfItem[5]  = Garage;           /* Oil */
    locationOfItem[6]  = Kitchen;          /* Candybar */
    locationOfItem[7]  = Driveway2;        /* Bottle */
    locationOfItem[8]  = GirlsBedroom;     /* Doll */
    locationOfItem[9]  = BoysBedroom;      /* ToyCar */
    locationOfItem[10] = ServantsQuarters; /* Matches */
    locationOfItem[11] = Woods25;          /* GoldCoin */
    locationOfItem[12] = Woods29;          /* SilverCoin */
    locationOfItem[13] = DiningRoom;       /* StaleMeat */
    locationOfItem[14] = DrawingRoom;      /* Book */
    locationOfItem[15] = LaundryRoom;      /* Cheese */
    locationOfItem[16] = MasterBedroom;    /* OldRadio */
}

/* Main program (obviously) */
int main()
{
    while (1) {
        initialize();
        clearScreen();

        printf("%s", "                      Abandoned Farmhouse Adventure\n");
        printf("%s", "                            By Jeff Tranter\n\n");
        printf("%s", "Your four-year-old grandson has gone missing and was last seen headed in the\n");
        printf("%s", "direction of the abandoned family farm. It's a dangerous place to play. You\n");
        printf("%s", "have to find him before he gets hurt, and it will be getting dark soon...\n\n");

        while (!gameOver) {
            prompt();
            if (buffer[0] == '\0') {
                ; /* do nothing for empty command line */
            } else if (buffer[0] == 'h') {
                doHelp();
            } else if (buffer[0] == 'i') {
                doInventory();
            } else if ((buffer[0] == 'g')
                       || !strcmp(buffer, "n") || !strcmp(buffer, "s")
                       || !strcmp(buffer, "e") || !strcmp(buffer, "w")
                       || !strcmp(buffer, "u") || !strcmp(buffer, "d")
                       || !strcmp(buffer, "north") || !strcmp(buffer, "south")
                       || !strcmp(buffer, "east") || !strcmp(buffer, "west")
                       || !strcmp(buffer, "up") || !strcmp(buffer, "down")) {
                doGo();
            } else if (buffer[0] == 'l') {
                doLook();
            } else if (buffer[0] == 't') {
                doTake();
            } else if (buffer[0] == 'e') {
                doExamine();
            } else if (buffer[0] == 'u') {
                doUse();
            } else if (buffer[0] == 'd') {
                doDrop();
            } else if (buffer[0] == 'q') {
                doQuit();
            } else if (!strcmp(buffer, "xyzzy")) {
                printf("Nice try, but that won't work here.\n");
            } else {
                printf("I don't understand. Try 'help'.\n");
            }

            /* Handle special actons. */
            doActions();
        }

        printf("Game over after %d turns.\n", turnsPlayed);
        printf("%s", "Do you want to play again (y/n)? ");
        fgets(buffer, SIZEOFBUFFER-1, stdin);
        if (tolower(buffer[0]) == 'n') {
            break;
        }
    }
    return 0;
}
