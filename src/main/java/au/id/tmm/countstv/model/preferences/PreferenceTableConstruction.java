package au.id.tmm.countstv.model.preferences;

import com.google.common.collect.ImmutableSortedSet;
import gnu.trove.map.TObjectIntMap;
import gnu.trove.map.hash.TObjectIntHashMap;

import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.SortedSet;

public final class PreferenceTableConstruction {

    private PreferenceTableConstruction() {
    }

    private static final float ALL_BALLOT_PAPERS_GROWTH_FACTOR = 1.5f;

    @SuppressWarnings("unchecked")
    public static <C> PreferenceTable<C> from(
            Iterable<Collection<C>> ballotsIterable,
            int numBallotsHint,
            Collection<C> allCandidates,
            Comparator<C> candidateOrdering
    ) {
        SortedSet<C> orderedCandidates = new ImmutableSortedSet.Builder<C>(candidateOrdering)
                .addAll(allCandidates)
                .build();

        C[] candidateLookup = (C[]) orderedCandidates.toArray();

        TObjectIntMap<C> candidateIndexLookup = buildCandidateIndexLookup(candidateLookup);

        int[][] allBallotsArray = readAllPreferencesIntoArray(ballotsIterable, numBallotsHint, candidateIndexLookup);

        int totalNumPapers = allBallotsArray.length;

        int[][] table = collapseRepeatedBallots(allBallotsArray);

        return new PreferenceTable<>(table, candidateLookup, totalNumPapers);
    }

    private static <C> TObjectIntMap<C> buildCandidateIndexLookup(C[] candidateLookup) {
        TObjectIntMap<C> candidateIndexLookup = new TObjectIntHashMap<>(candidateLookup.length);

        for (int i = 0; i < candidateLookup.length; i++) {
            candidateIndexLookup.put(candidateLookup[i], i);
        }

        return candidateIndexLookup;
    }

    @SuppressWarnings("unchecked")
    private static <C> int[][] readAllPreferencesIntoArray(
            Iterable<Collection<C>> ballotsIterable,
            int numBallotsHint,
            TObjectIntMap<C> candidateIndexLookup
    ) {
        int[][] allBallotPapers = new int[numBallotsHint][];
        int i = 0;

        for (Collection<C> ballotPaper : ballotsIterable) {
            C[] preferences = (C[]) ballotPaper.toArray();

            int[] preferencesAsInts = convertToCandidateInts(preferences, candidateIndexLookup);

            if (i + 1 > allBallotPapers.length) {
                allBallotPapers = Arrays.copyOf(allBallotPapers, (int) (allBallotPapers.length * ALL_BALLOT_PAPERS_GROWTH_FACTOR));
            }

            allBallotPapers[i++] = preferencesAsInts;
        }

        allBallotPapers = Arrays.copyOf(allBallotPapers, i);

        Arrays.parallelSort(allBallotPapers, new IntArrayComparator());

        return allBallotPapers;
    }

    private static <C> int[] convertToCandidateInts(C[] preferences, TObjectIntMap<C> candidateIndexLookup) {
        int[] preferencesAsInts = new int[preferences.length];

        for (int i = 0; i < preferences.length; i++) {
            preferencesAsInts[i] = candidateIndexLookup.get(preferences[i]);
        }

        return preferencesAsInts;
    }

    private static int[][] collapseRepeatedBallots(int[][] allBallotPapers) {
        int[][] table = new int[allBallotPapers.length][];
        int tableIndex = 0;

        int[] previousPreferenceArray = null;

        for (int[] thisPreferenceArray : allBallotPapers) {
            if (previousPreferenceArray == null || !Arrays.equals(thisPreferenceArray, previousPreferenceArray)) {
                int[] newTableRow = new int[thisPreferenceArray.length + 1];
                newTableRow[0] = 1;
                System.arraycopy(thisPreferenceArray, 0, newTableRow, 1, thisPreferenceArray.length);

                table[tableIndex++] = newTableRow;
            } else {
                table[tableIndex - 1][0] += 1;
            }

            previousPreferenceArray = thisPreferenceArray;
        }

        if (tableIndex != table.length) {
            table = Arrays.copyOf(table, tableIndex);
        }

        return table;
    }

    private static final class IntArrayComparator implements Comparator<int[]> {
        @Override
        public int compare(int[] left, int[] right) {
            if (left == right) {
                return 0;
            }

            for (int i = 0; i < Math.min(left.length, right.length); i++) {
                int comparison = Integer.compare(left[i], right[i]);

                if (comparison != 0) {
                    return comparison;
                }
            }

            return Integer.compare(left.length, right.length);
        }
    }
}
