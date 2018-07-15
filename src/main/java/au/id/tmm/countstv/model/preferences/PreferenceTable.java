package au.id.tmm.countstv.model.preferences;

import scala.Option;
import scala.collection.immutable.List;
import scala.collection.mutable.Builder;

import java.util.Arrays;
import java.util.Objects;

class PreferenceTable<C> {

    private final int[][] table;
    private final C[] candidateLookup;
    private final int totalNumPapers;

    public PreferenceTable(int[][] table, C[] candidateLookup, int totalNumPapers) {
        this.table = table;
        this.candidateLookup = candidateLookup;
        this.totalNumPapers = totalNumPapers;
    }

    public View<C> rootView() {
        return new View<>(this, 0, table.length, -1, totalNumPapers);
    }

    C[] getCandidateLookup() {
        return candidateLookup;
    }

    int getTotalNumPapers() {
        return totalNumPapers;
    }

    int[][] getTable() {
        return table;
    }

    public static final class View<C> {
        private final PreferenceTable<C> preferenceTable;

        private final int viewStartIndex;
        private final int viewEndIndex;

        private final int preferenceIndex;

        public final int numPapers;

        private View(
                PreferenceTable<C> preferenceTable,
                int viewStartIndex,
                int viewEndIndex,
                int preferenceIndex,
                int numPapers
        ) {
            this.preferenceTable = preferenceTable;
            this.viewStartIndex = viewStartIndex;
            this.viewEndIndex = viewEndIndex;
            this.preferenceIndex = preferenceIndex;
            this.numPapers = numPapers;
        }

        public Option<C> assignedCandidate() {
            if (preferenceIndex < 0) {
                return Option.<C>empty();
            } else {
                int candidateIndex = preferenceTable.table[viewStartIndex][preferenceIndex + 1];
                C candidate = preferenceTable.candidateLookup[candidateIndex];
                return Option.<C>apply(candidate);
            }
        }

        public scala.collection.immutable.List<View<C>> children() {
            Builder<View<C>, List<View<C>>> childrenBuilder = List.<View<C>>canBuildFrom().apply();

            int preferenceIndexForChildren = preferenceIndex + 1;

            int previousCandidate = UNINITIALISED;
            int numPapersForPreviousCandidate = 0;
            int startIndexForPreviousCandidate = viewStartIndex;

            for (int i = viewStartIndex; i < viewEndIndex; i++) {
                int[] tableRow = preferenceTable.table[i];

                int currentCandidate;
                if (preferenceIndexForChildren + 1 < tableRow.length) {
                    currentCandidate = tableRow[preferenceIndexForChildren + 1];
                } else {
                    currentCandidate = EXHAUSTED;
                }

                if (previousCandidate != UNINITIALISED && previousCandidate != currentCandidate) {
                    if (previousCandidate != EXHAUSTED) {
                        View<C> newView = new View<>(
                                preferenceTable,
                                startIndexForPreviousCandidate,
                                i,
                                preferenceIndexForChildren,
                                numPapersForPreviousCandidate
                        );

                        childrenBuilder.$plus$eq(newView);
                    }

                    numPapersForPreviousCandidate = 0;
                    startIndexForPreviousCandidate = i;
                }

                previousCandidate = currentCandidate;
                numPapersForPreviousCandidate += tableRow[0];
            }

            if (previousCandidate != UNINITIALISED && previousCandidate != EXHAUSTED) {
                View<C> finalView = new View<>(
                        preferenceTable,
                        startIndexForPreviousCandidate,
                        viewEndIndex,
                        preferenceIndexForChildren,
                        numPapersForPreviousCandidate
                );

                childrenBuilder.$plus$eq(finalView);
            }

            return childrenBuilder.result();
        }

        private static final int EXHAUSTED = -1;
        private static final int UNINITIALISED = -2;

        @Override
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            View<?> view = (View<?>) o;
            return viewStartIndex == view.viewStartIndex &&
                    viewEndIndex == view.viewEndIndex &&
                    preferenceIndex == view.preferenceIndex &&
                    Objects.equals(preferenceTable, view.preferenceTable);
        }

        @Override
        public int hashCode() {
            return Objects.hash(preferenceTable, viewStartIndex, viewEndIndex, preferenceIndex);
        }
    }

    public boolean equalTo(PreferenceTable<C> that) {
        return this.totalNumPapers == that.totalNumPapers &&
                Arrays.equals(this.candidateLookup, that.candidateLookup) &&
                Arrays.deepEquals(this.table, that.table);
    }

}
