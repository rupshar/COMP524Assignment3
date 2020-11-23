val SMALL_DISTANCE = 6;
val MEDIUM_DISTANCE = 13;
val LARGE_DISTANCE = 27;

val SMALL_DURATION = 15;
val MEDIUM_DURATION = 30;
val LARGE_DURATION = 120;

val SMALL_EXHALATION = 10;
val MEDIUM_EXHALATION = 30;
val LARGE_EXHALATION = 50;

val MAX = 200;
val ZERO = 0;

val SAFETY_TABLE = [(13,30,30),(6,30,10),(27,30,50),(13,15,50),(13,120,10),(27,120,30),(6,15,30)];

fun interpolateDistance(distance, low, medium, high) = 
	if distance < low then ZERO else
	if distance >= high then LARGE_DISTANCE else
	if distance < high andalso distance >= medium then MEDIUM_DISTANCE
	else SMALL_DISTANCE;

fun interpolateDuration(duration, low, medium, high) =
	if duration <= low then SMALL_DURATION else
	if duration > high then MAX else
	if duration > medium andalso duration <= high then LARGE_DURATION
	else MEDIUM_DURATION;
	
fun interpolateExhalation(exhalation, low, medium, high) =
	if exhalation <= low then SMALL_EXHALATION else
	if exhalation > high then MAX else
	if exhalation > medium andalso exhalation <= high then LARGE_EXHALATION
	else MEDIUM_EXHALATION;

fun givenSafe(distance, duration, exhalation) = 
	(distance = MEDIUM_DISTANCE andalso duration = MEDIUM_DURATION andalso exhalation = MEDIUM_EXHALATION)
	orelse
	(distance = SMALL_DISTANCE andalso duration = MEDIUM_DURATION andalso exhalation = SMALL_EXHALATION)
	orelse
	(distance = LARGE_DISTANCE andalso duration = MEDIUM_DURATION andalso exhalation = LARGE_EXHALATION)
	orelse
	(distance = MEDIUM_DISTANCE andalso duration = SMALL_DURATION andalso exhalation = LARGE_EXHALATION)
	orelse
	(distance = MEDIUM_DISTANCE andalso duration = LARGE_DURATION andalso exhalation = SMALL_EXHALATION)
	orelse
	(distance = LARGE_DISTANCE andalso duration = LARGE_DURATION andalso exhalation = MEDIUM_EXHALATION)
	orelse
	(distance = SMALL_DISTANCE andalso duration = SMALL_DURATION andalso exhalation = MEDIUM_EXHALATION);
	
fun interpolatedSafe(distance, duration, exhalation) = 
	let 
		val interpolatedDistance = interpolateDistance(distance, SMALL_DISTANCE, MEDIUM_DISTANCE, LARGE_DISTANCE)
		val interpolatedDuration = interpolateDuration(duration, SMALL_DURATION, MEDIUM_DURATION, LARGE_DURATION)
		val interpolatedExhalation = interpolateExhalation(exhalation, SMALL_EXHALATION, MEDIUM_EXHALATION, LARGE_EXHALATION)
	in
		givenSafe(interpolatedDistance, interpolatedDuration, interpolatedExhalation)
	end

fun
	derivedIteration (distance, duration, exhalation, nil) = false |
	derivedIteration (distance, duration, exhalation, ((safeDist, safeDur, safeEx) :: safetyTail)) =
		if (distance >= safeDist andalso duration <= safeDur andalso exhalation <= safeEx) then
			true
		else
			derivedIteration (distance, duration, exhalation, safetyTail);
	
fun listDerivedSafe (distance, duration, exhalation) =
	derivedIteration(distance, duration, exhalation, SAFETY_TABLE);
	
fun printSafety (safetyComputer, (distance, duration, exhalation)) =
	let
		val isSafe = safetyComputer(distance, duration, exhalation)	
	in
		print ("Distance:"^Int.toString distance^" Duration:"^Int.toString duration^" Exhalation:"^Int.toString exhalation^" Safe:"^Bool.toString isSafe^"\n")
	end
	
fun concisePrintSafety (safetyComputer, (distance, duration, exhalation)) =
	let
		val isSafe = safetyComputer(distance, duration, exhalation)	
	in
		print ("("^Int.toString distance^","^Int.toString duration^","^Int.toString exhalation^","^Bool.toString isSafe^")\n")
	end
	
fun
	printIterator(printSafety, safetyComputer, nil) = () | 
	printIterator(printSafety, safetyComputer, (head :: tail)) =
	(	
		printSafety(safetyComputer, head) ;
		printIterator(printSafety, safetyComputer, tail)
	);	
	
fun listPrintSafety(printSafety, safetyComputer, listToPrint) =
	printIterator(printSafety, safetyComputer, listToPrint);
	
fun
	matchingSafeIterator(matcherFunction, (distance, duration, exhalation), nil) = false |
	matchingSafeIterator(matcherFunction, (distance, duration, exhalation), ((safeDist, safeDur, safeEx) :: safeTail)) =
		if matcherFunction((distance, duration, exhalation), (safeDist, safeDur, safeEx)) then
			true
		else
			matchingSafeIterator(matcherFunction, (distance, duration, exhalation), safeTail);
	
fun matchingSafe(matcherFunction, (distance, duration, exhalation)) =
	matchingSafeIterator(matcherFunction, (distance, duration, exhalation), SAFETY_TABLE);
	
fun derivedSafeMatcher((distance, duration, exhalation), (safeDist, safeDur, safeEx)) =
	if (distance >= safeDist andalso duration <= safeDur andalso exhalation <= safeEx) then
		true
	else
		false;	
		
fun givenSafeMatcher((distance, duration, exhalation), (safeDist, safeDur, safeEx)) =
	if (distance = safeDist andalso duration = safeDur andalso exhalation = safeEx) then
		true
	else 
		false
		
fun matchingDerivedSafe(distance, duration, exhalation) =
	matchingSafe(derivedSafeMatcher, (distance, duration, exhalation));
	
fun matchingGivenSafe(distance, duration, exhalation) =
	matchingSafe(givenSafeMatcher, (distance, duration, exhalation));
	
fun curryableInterpolatedSafe distance duration exhalation =
	interpolatedSafe(distance, duration, exhalation);
	
fun curriedOnceInterpolatedSafe duration exhalation = 
	let
		val distance = MEDIUM_DISTANCE
	in
		curryableInterpolatedSafe distance duration exhalation
	end
	
fun curriedTwiceInterpolatedSafe exhalation =
	let
		val duration = MEDIUM_DURATION
	in
		curriedOnceInterpolatedSafe duration exhalation
	end
		
fun curryableMatchingSafe matcherFunction (distance,duration,exhalation) =
	matchingSafe(matcherFunction, (distance, duration, exhalation));
		
fun curriedMatchingDerivedSafe (distance,duration,exhalation) =
	curryableMatchingSafe derivedSafeMatcher (distance,duration,exhalation);
	
fun curriedMatchingGivenSafe (distance,duration,exhalation) =
	curryableMatchingSafe givenSafeMatcher (distance,duration,exhalation);
	
