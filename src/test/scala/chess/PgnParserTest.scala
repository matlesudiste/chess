package chess

import org.scalatest.FunSuite


class PgnParserTest extends FunSuite {
	
	test("parse tags") {
		val pgn = "[Event \"Kasparov Simul 2016\"] [Site \"Moenchengladbach GER\"]"
		val result = PgnParser.parseTags(pgn)

		assert(!result.isEmpty)
		assert(result.get("Event") === "Kasparov Simul 2016")
		assert(result.get("Site") === "Moenchengladbach GER")
	}

	test("parse moves") {
		val pgn = """1. d4 Nf6 2. Bg5 e6 3. e4 Be7 4. Nd2 d5 5. e5 Nfd7 6. Be3 b6 7. Nh3 Bb7 8. Qg4
					g6 9. Ng5 Nf8 10. h4 h6 11. Ngf3 Na6 12. c3 Qd7 13. a4 O-O-O 14. b4 c5 15. Bb5
					Qc7 16. dxc5 bxc5 17. O-O f5 18. exf6 Bxf6 19. Rac1 h5 20. Qh3 Kb8 21. Rfe1 Bc8
					22. Bxa6 Bxa6 23. Bxc5 Bc8 24. c4 Be7 25. Bxe7 Qxe7 26. Qg3+ Qd6 27. Ne5 Rd7
					28. Nxd7+ Kc7 29. Ne5 Kb7 30. c5 Qc7 31. c6+ Ka8 32. b5 Rh7 33. Qf4 Rh8 34.
					Ndf3 a6 35. Nd4 Nh7 36. bxa6 Rf8 37. Qe3 Bxa6 38. Nb5 Bxb5 39. axb5 Kb8 40. b6
					Qe7 41. Ra1 Qd6 42. Nd7+ Kc8 43. Ra8+ 1-0"""

		val result = PgnParser.parseMoves(pgn)

		assert(!result.isEmpty)
		assert(result.get.size === 85)

		assert(result.get(0) === "d4")
		assert(result.get(25) === "O-O-O")
		assert(result.get(30) === "dxc5")
		assert(result.get(84) === "Ra8+")
	}

	test("parse single game") {
		val pgn = """[Event "Kasparov Simul 2016"]
					[Site "Moenchengladbach GER"]

					1. e4 c5 2. Nf3 Nc6 3. Bb5 g6 4. Bxc6 dxc6 5. d3 Bg7 6. h3 Nf6 7. Nc3 Qc7 8.
					Be3 b6 9. Qd2 h6 10. Bf4 Qb7 11. O-O g5 12. Be5 Be6 13. a3 O-O-O 14. b4 c4 15.
					Qe2 cxd3 16. cxd3 Qd7 17. d4 g4 18. hxg4 Bxg4 19. a4 h5 20. Rfc1 Kb7 21. a5 Ra8
					22. d5 Bxf3 23. Qxf3 cxd5 24. a6+ Kc8 25. Nxd5+ Kd8 26. Nxf6 Bxf6 27. Bxf6 Rh6
					28. e5 Rb8 29. Rd1 exf6 30. Qc6 Ke8 31. Rxd7 fxe5 32. Qc7 Kf8 33. Qxb8+ 1-0"""

		val result = PgnParser.parseSingleGame(pgn)

		assert(!result.isEmpty)

		val moves = result.get._2

		assert(moves.size === 65)
		assert(moves(0) === "e4")
		assert(moves(64) === "Qxb8+")
	}

	test("parse multiple games") {
		val pgn = """[Event "Kasparov Simul 2016"]
					[Site "Moenchengladbach GER"]
					1. d4 Nf6 2. Bg5 e6 3. e4 Be7 4. Nd2 d5 5. e5 Nfd7 6. Be3 b6 7. Nh3 Bb7 8. Qg4
					g6 9. Ng5 Nf8 10. h4 h6 11. Ngf3 Na6 12. c3 Qd7 13. a4 O-O-O 14. b4 c5 15. Bb5
					Qc7 16. dxc5 bxc5 17. O-O f5 18. exf6 Bxf6 19. Rac1 h5 20. Qh3 Kb8 21. Rfe1 Bc8
					22. Bxa6 Bxa6 23. Bxc5 Bc8 24. c4 Be7 25. Bxe7 Qxe7 26. Qg3+ Qd6 27. Ne5 Rd7
					28. Nxd7+ Kc7 29. Ne5 Kb7 30. c5 Qc7 31. c6+ Ka8 32. b5 Rh7 33. Qf4 Rh8 34.
					Ndf3 a6 35. Nd4 Nh7 36. bxa6 Rf8 37. Qe3 Bxa6 38. Nb5 Bxb5 39. axb5 Kb8 40. b6
					Qe7 41. Ra1 Qd6 42. Nd7+ Kc8 43. Ra8+ 1-0

					[Event "Kasparov Simul 2016"]
					[Site "Moenchengladbach GER"]

					1. e4 c5 2. Nf3 Nc6 3. Bb5 g6 4. Bxc6 dxc6 5. d3 Bg7 6. h3 Nf6 7. Nc3 Qc7 8.
					Be3 b6 9. Qd2 h6 10. Bf4 Qb7 11. O-O g5 12. Be5 Be6 13. a3 O-O-O 14. b4 c4 15.
					Qe2 cxd3 16. cxd3 Qd7 17. d4 g4 18. hxg4 Bxg4 19. a4 h5 20. Rfc1 Kb7 21. a5 Ra8
					22. d5 Bxf3 23. Qxf3 cxd5 24. a6+ Kc8 25. Nxd5+ Kd8 26. Nxf6 Bxf6 27. Bxf6 Rh6
					28. e5 Rb8 29. Rd1 exf6 30. Qc6 Ke8 31. Rxd7 fxe5 32. Qc7 Kf8 33. Qxb8+ 1-0"""
					
		val result = PgnParser.parseMultipleGames(pgn)

		assert(!result.isEmpty)

		assert(result.get.size === 2)
		assert(result.get(0)._2.size === 85)
		assert(result.get(0)._2(1) === "Nf6")
		assert(result.get(1)._2.size === 65)
		assert(result.get(1)._2(3) === "Nc6")
	}

}
