/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.api;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Arrays;

import org.junit.jupiter.api.Test;

import io.vertx.core.json.JsonArray;

/**
 * Test the {@link Operations}.
 *
 * @see Operations
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class OperationsTest {

	/**
	 * Check that convert a {@code null} array to a {@©ode null} list.
	 */
	@Test
	public void shouldNullArrayBeNullListString() {

		assertThat(Operations.toListString(null)).isNull();

	}

	/**
	 * Check that convert an empty array to a {@©ode null} list.
	 */
	@Test
	public void shouldEmptyArrayBeNullListString() {

		assertThat(Operations.toListString(new JsonArray())).isNull();

	}

	/**
	 * Check that convert an empty array to a string list.
	 */
	@Test
	public void shouldArrayBeConvertedToListString() {

		assertThat(Operations.toListString(new JsonArray().add("value").add("true").add("3")))
				.isEqualTo(Arrays.asList("value", "true", "3"));

	}

}
