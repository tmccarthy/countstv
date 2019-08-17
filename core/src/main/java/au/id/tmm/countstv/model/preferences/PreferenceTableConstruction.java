package au.id.tmm.countstv.model.preferences;

import com.google.common.collect.ImmutableSortedSet;
import gnu.trove.map.TObjectIntMap;
import gnu.trove.map.hash.TObjectIntHashMap;
import scala.Tuple2;

import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.SortedSet;

public final class PreferenceTableConstruction {

    private PreferenceTableConstruction() {
    }

    private static final float ALL_BALLOT_PAPERS_GROWTH_FACTOR = 1.5f;

    @SuppressWarnings("unchecked")
    public static <C> PreferenceTable<C> from(
            Iterator<Collection<C>> ballotsIterable,
            int numBallotsHint,
            Collection<C> allCandidates,
            Comparator<C> candidateOrdering
    ) {
        SortedSet<C> orderedCandidates = new ImmutableSortedSet.Builder<C>(candidateOrdering)
                .addAll(allCandidates)
                .build();

        C[] candidateLookup = (C[]) orderedCandidates.toArray();

        TObjectIntMap<C> candidateIndexLookup = buildCandidateIndexLookup(candidateLookup);

        short[][] allBallotsArray = readAllPreferencesIntoArray(ballotsIterable, numBallotsHint, candidateIndexLookup);

        int totalNumPapers = allBallotsArray.length;

        Tuple2<int[], short[][]> table = collapseRepeatedBallots(allBallotsArray);

        return new PreferenceTable<>(table._1, table._2, candidateLookup, totalNumPapers);
    }

    private static <C> TObjectIntMap<C> buildCandidateIndexLookup(C[] candidateLookup) {
        TObjectIntMap<C> candidateIndexLookup = new TObjectIntHashMap<>(candidateLookup.length);

        for (int i = 0; i < candidateLookup.length; i++) {
            candidateIndexLookup.put(candidateLookup[i], i);
        }

        return candidateIndexLookup;
    }

    @SuppressWarnings("unchecked")
    private static <C> short[][] readAllPreferencesIntoArray(
            Iterator<Collection<C>> ballotsIterator,
            int numBallotsHint,
            TObjectIntMap<C> candidateIndexLookup
    ) {
        short[][] allBallotPapers = new short[numBallotsHint][];
        int i = 0;

        while(ballotsIterator.hasNext()) {
            Collection<C> ballotPaper = ballotsIterator.next();

            C[] preferences = (C[]) ballotPaper.toArray();

            short[] preferencesAsInts = convertToCandidateInts(preferences, candidateIndexLookup);

            if (i + 1 > allBallotPapers.length) {
                allBallotPapers = Arrays.copyOf(allBallotPapers, (int) (allBallotPapers.length * ALL_BALLOT_PAPERS_GROWTH_FACTOR));
            }

            allBallotPapers[i++] = preferencesAsInts;
        }

        allBallotPapers = Arrays.copyOf(allBallotPapers, i);

        Arrays.parallelSort(allBallotPapers, new ShortArrayComparator());

        return allBallotPapers;
    }

    private static <C> short[] convertToCandidateInts(C[] preferences, TObjectIntMap<C> candidateIndexLookup) {
        short[] preferencesAsInts = new short[preferences.length];

        for (int i = 0; i < preferences.length; i++) {
            preferencesAsInts[i] = (short) candidateIndexLookup.get(preferences[i]);
        }

        return preferencesAsInts;
    }

    private static Tuple2<int[], short[][]> collapseRepeatedBallots(short[][] allBallotPapers) {
        int[] rowPaperCounts = new int[allBallotPapers.length];
        short[][] preferenceArrays = new short[allBallotPapers.length][];

        int tableIndex = 0;

        short[] previousPreferenceArray = null;

        for (short[] thisPreferenceArray : allBallotPapers) {
            if (previousPreferenceArray == null || !Arrays.equals(thisPreferenceArray, previousPreferenceArray)) {
                short[] newPreferenceArray = new short[thisPreferenceArray.length];

                System.arraycopy(thisPreferenceArray, 0, newPreferenceArray, 0, thisPreferenceArray.length);

                rowPaperCounts[tableIndex] = 1;
                preferenceArrays[tableIndex] = newPreferenceArray;

                tableIndex++;
            } else {
                rowPaperCounts[tableIndex -1] += 1;
            }

            previousPreferenceArray = thisPreferenceArray;
        }

        if (tableIndex != rowPaperCounts.length) {
            rowPaperCounts = Arrays.copyOf(rowPaperCounts, tableIndex);
            preferenceArrays = Arrays.copyOf(preferenceArrays, tableIndex);
        }

        return Tuple2.<int[], short[][]>apply(rowPaperCounts, preferenceArrays);
    }

    private static final class ShortArrayComparator implements Comparator<short[]> {
        @Override
        public int compare(short[] left, short[] right) {
            if (left == right) {
                return 0;
            }

            for (int i = 0; i < Math.min(left.length, right.length); i++) {
                int comparison = Short.compare(left[i], right[i]);

                if (comparison != 0) {
                    return comparison;
                }
            }

            return Integer.compare(left.length, right.length);
        }
    }
}
