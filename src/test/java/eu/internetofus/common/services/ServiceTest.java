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

package eu.internetofus.common.services;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import eu.internetofus.common.services.Service;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpServerResponse;
import io.vertx.core.json.DecodeException;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.WebClient;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import io.vertx.junit5.web.VertxWebClientExtension;

/**
 * Test the {@link Service}
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(VertxExtension.class)
@ExtendWith(VertxWebClientExtension.class)
public class ServiceTest {

	/**
	 * Verify that can not post over an undefined service.
	 *
	 * @param client      to use.
	 * @param testContext context that manage the test.
	 */
	@Test
	public void shouldNotPostOverAnUndefinedService(WebClient client, VertxTestContext testContext) {

		final JsonObject options = new JsonObject().put("host", "undefined").put("port", 0).put("apiPath", "/udefined");
		final Service service = new Service(client, options);
		service.post("path", new JsonObject(), testContext.failing(ignored -> testContext.completeNow()));

	}

	/**
	 * Verify that fail post when not return a JSON object.
	 *
	 * @param vertx       platform that manage the event bus.
	 * @param client      to use.
	 * @param testContext context that manage the test.
	 */
	@Test
	public void shouldFailPostIfNotReturnJsonObject(Vertx vertx, WebClient client, VertxTestContext testContext) {

		vertx.createHttpServer().requestHandler(request -> {
			final HttpServerResponse response = request.response();
			response.putHeader("content-type", "text/plain");
			response.end("Hello World!");
		}).listen(0, "localhost", testContext.succeeding(server -> {

			final JsonObject options = new JsonObject().put("host", "localhost").put("port", server.actualPort())
					.put("apiPath", "/api");
			final Service service = new Service(client, options);
			service.post("path", new JsonObject(), testContext.failing(cause -> {

				server.close();
				assertThat(cause).isInstanceOf(DecodeException.class);
				testContext.completeNow();

			}));

		}));

	}

	/**
	 * Verify that can not get over an undefined service.
	 *
	 * @param client      to use.
	 * @param testContext context that manage the test.
	 */
	@Test
	public void shouldNotGetOverAnUndefinedService(WebClient client, VertxTestContext testContext) {

		final JsonObject options = new JsonObject().put("host", "undefined").put("port", 0).put("apiPath", "/udefined");
		final Service service = new Service(client, options);
		service.get("path", testContext.failing(ignored -> testContext.completeNow()));

	}

	/**
	 * Verify that can not delete over an undefined service.
	 *
	 * @param client      to use.
	 * @param testContext context that manage the test.
	 */
	@Test
	public void shouldNotDeleteOverAnUndefinedService(WebClient client, VertxTestContext testContext) {

		final JsonObject options = new JsonObject().put("host", "undefined").put("port", 0).put("apiPath", "/udefined");
		final Service service = new Service(client, options);
		service.delete("path", testContext.failing(ignored -> testContext.completeNow()));

	}

}
