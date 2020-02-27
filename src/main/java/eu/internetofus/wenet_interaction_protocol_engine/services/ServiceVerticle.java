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

package eu.internetofus.wenet_interaction_protocol_engine.services;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.WebClient;
import io.vertx.ext.web.client.WebClientOptions;

/**
 * The verticle that provide the services to interact with the other WeNet
 * modules.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class ServiceVerticle extends AbstractVerticle {

	/**
	 * The component to do request to other services.
	 */
	protected WebClient client;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void start(Promise<Void> startPromise) throws Exception {

		try {

			final JsonObject serviceConf = this.config().getJsonObject("service", new JsonObject());

			// configure the web client
			final JsonObject webClientConf = serviceConf.getJsonObject("webClient", new JsonObject());
			final WebClientOptions options = new WebClientOptions(webClientConf);
			this.client = WebClient.create(this.vertx, options);

			// register the service to interact with the profile manager
			final JsonObject profileManagerConf = serviceConf.getJsonObject("profileManager", new JsonObject());
			WeNetProfileManagerService.register(this.vertx, this.client, profileManagerConf);

			startPromise.complete();

		} catch (final Throwable cause) {

			startPromise.fail(cause);
		}
	}

	/**
	 * Close the web client.
	 *
	 * {@inheritDoc}
	 *
	 * @see #client
	 */
	@Override
	public void stop() {

		if (this.client != null) {

			this.client.close();
			this.client = null;
		}
	}

}
