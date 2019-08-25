package au.id.tmm.countstv.model.preferences;

import scala.Option;
import scala.collection.immutable.ArraySeq;
import scala.collection.mutable.Builder;

import java.util.Arrays;
import java.util.Objects;

class PreferenceTable<C> {

    private final int[] rowPaperCounts;
    private final short[][] preferenceArrays;
    private final C[] candidateLookup;
    private final int totalNumPapers;

    public PreferenceTable(int[] rowPaperCounts, short[][] preferenceArrays, C[] candidateLookup, int totalNumPapers) {
        this.rowPaperCounts = rowPaperCounts;
        this.preferenceArrays = preferenceArrays;
        this.candidateLookup = candidateLookup;
        this.totalNumPapers = totalNumPapers;
    }

    public View<C> rootView() {
        return new View<>(this, 0, rowPaperCounts.length, -1, totalNumPapers);
    }

    C[] getCandidateLookup() {
        return candidateLookup;
    }

    int getTotalNumPapers() {
        return totalNumPapers;
    }

    int getLength() {
        return rowPaperCounts.length;
    }

    int[] getRowPaperCounts() {
        return rowPaperCounts;
    }

    short[][] getPreferenceArrays() {
        return preferenceArrays;
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

        @SuppressWarnings("unchecked")
        public ArraySeq<C> path() {
            Object[] path = new Object[preferenceIndex];

            for (int i = 0; i < preferenceIndex; i++) {
                path[i] = candidateAtPreferenceIndex(i);
            }

            return ArraySeq.unsafeWrapArray((C[]) path);
        }

        public Option<C> assignedCandidate() {
            if (preferenceIndex < 0) {
                return Option.<C>empty();
            } else {
                return Option.<C>apply(candidateAtPreferenceIndex(preferenceIndex));
            }
        }

        private C candidateAtPreferenceIndex(int index) {
            int candidateIndex = preferenceTable.preferenceArrays[viewStartIndex][index];
            C candidate = preferenceTable.candidateLookup[candidateIndex];
            return candidate;
        }

        @SuppressWarnings({"unchecked", "rawtypes"})
        public ArraySeq<View<C>> children() {
            Builder<View<C>, ArraySeq> childrenBuilder = ArraySeq.untagged().<View<C>>newBuilder();

            int preferenceIndexForChildren = preferenceIndex + 1;

            int previousCandidate = UNINITIALISED;
            int numPapersForPreviousCandidate = 0;
            int startIndexForPreviousCandidate = viewStartIndex;

            for (int i = viewStartIndex; i < viewEndIndex; i++) {
                int numPapers = preferenceTable.rowPaperCounts[i];
                short[] preferenceArray = preferenceTable.preferenceArrays[i];

                int currentCandidate;
                if (preferenceIndexForChildren < preferenceArray.length) {
                    currentCandidate = preferenceArray[preferenceIndexForChildren];
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
                numPapersForPreviousCandidate += numPapers;
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
            @SuppressWarnings("unchecked") View<C> view = (View<C>) o;
            return viewStartIndex == view.viewStartIndex &&
                    viewEndIndex == view.viewEndIndex &&
                    preferenceIndex == view.preferenceIndex &&
                    preferenceTable.equalTo(view.preferenceTable);
        }

        @Override
        public int hashCode() {
            return Objects.hash(preferenceTable, viewStartIndex, viewEndIndex, preferenceIndex);
        }
    }

    public boolean equalTo(PreferenceTable<C> that) {
        return this.totalNumPapers == that.totalNumPapers &&
                Arrays.equals(this.candidateLookup, that.candidateLookup) &&
                Arrays.equals(this.rowPaperCounts, that.rowPaperCounts) &&
                Arrays.deepEquals(this.preferenceArrays, that.preferenceArrays);
    }

}
