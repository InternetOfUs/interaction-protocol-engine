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

package eu.internetofus.wenet_interaction_protocol_engine;

import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;

/**
 * Contains the information of a WeNet interaction protocol engine that has been
 * started.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class WeNetInteractionProtocolEngineContext {

	/**
	 * The component that manage the started vertices.
	 */
	public Vertx vertx;

	/**
	 * The configuration that has been used to start the interaction protocol
	 * engine.
	 */
	public JsonObject configuration;

	/**
	 * Create a new context.
	 *
	 * @param vertx         that manage the vertices.
	 * @param configuration the effective configuration used to start the wenet
	 *                      interaction protocol engine.
	 */
	public WeNetInteractionProtocolEngineContext(Vertx vertx, JsonObject configuration) {

		this.vertx = vertx;
		this.configuration = configuration;

	}

}
