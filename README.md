*Chainshot*
---
Investigating Artificial Intelligence techniques with a LISP implementation of the classic game *Samegame* (see one version [here](http://www.oopixel.com/games/samegame/)). The game basically consists of clearing similarly colored groups of beads that are horizontally or vertically adjacent on a 2D game board (see below). The game ends when there are no more groups of beads remaining on the board or there are no beads on the board at all.

![Screenshot](https://github.com/neeraj2608/chainshotAI/blob/master/chainshot.png)

Note that although the game appears simple, it is computationally *not*, primarily because there exist multiple end states of the game depending on the choices made at any step of the game.

This implementation provides a manual and an automatic gameplay mode (only the lattter is really of interest to computer scientists). Four gameplay modes based on different AI techniques were implemented and compared using two main criteria:

* runtime (with a cap of 30 seconds)
* the number of beads left on the board at the end of the game (or when the 30 seconds ran out)

Comparisons were made not only across algorithms, but also across different board sizes. The size of the boards I used ranged from 5x5 to 8x8.

These are the automatic gameplay modes:

1. **Intuitive search:** This is a human-like playing approach that I deduced after playing multiple rounds of different starting permutations of the game. The idea is that we target the largest groups on the board first so as to maximize our score (which is calculated based on the number of beads we remove with every move). Ironically, this minimally complex search method had the best results out of the 4 algorithms I implemented.
2. **Best-first Search:** In this approach, we make a list of all the groups on the board at the current time. Then, we remove each group turn by turn and see how many groups are left behind. Note that after doing this exercise for the first group, we restore the game position to the way it was before removing the second group and seeing the effect it has. In other words, the effect of removing a group is investigated independent of removing the other groups. Once we go through all the groups in this manner, we pick the group that leaves the fewest groups behind. This way we are left in the best position in the next move as we will have the most number of groups to choose from for the next iteration of the algorithm. The results of this group were slightly worse than Intuitive search.
3. **Depth-First Iterative Deepening:** This is standard depth-first iterative deepening. We start off with a depth of 1 and do a breadth-first search across the whole search tree. So, we go through all the groups on the board at the moment, removing them successively (independently of each other as in the last approach). With every removal, we check if the game is over. If not, we increase the depth by 1 and start from the beginning board position again. This approach performed worse than Best-First search and worked only for the 2 smallest board sizes. For the larger boards, the algorithm takes too long to run.
4. **Blind Depth-First Search:** This is standard blind depth-first search. Being an undirected search method, we are guaranteed to find a solution. This algorithm performs better than depth-first iterative deepening and in one case, outperformed intuitive and best-first search in terms of the number of beads left behind. However, like depth-first iterative deepening, this approach takes too long for larger board sizes. It terminated in time only for the 2 smallest boards.
