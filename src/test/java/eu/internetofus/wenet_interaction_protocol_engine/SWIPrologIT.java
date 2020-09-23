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
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
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

import static eu.internetofus.common.components.service.WeNetServiceSimulators.waitUntilCallbacks;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import eu.internetofus.common.components.StoreServices;
import eu.internetofus.common.components.interaction_protocol_engine.Message;
import eu.internetofus.common.components.interaction_protocol_engine.Message.Type;
import eu.internetofus.common.components.interaction_protocol_engine.WeNetInteractionProtocolEngine;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;

/**
 * Check the integration for the SWIProlog.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class SWIPrologIT {

  /**
   * The engine has to process the SWIProlog message.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  public void shoulProcessSwiPrologMessage(final Vertx vertx, final VertxTestContext testContext) {

    StoreServices.storeAppExample(1, vertx, testContext, testContext.succeeding(app -> {

      final var message = new Message();
      message.type = Type.SWI_PROLOG;
      message.appId = app.appId;
      message.content = new JsonObject();
      WeNetInteractionProtocolEngine.createProxy(vertx).sendMessage(message, testContext.succeeding(sentMessage -> {

        waitUntilCallbacks(app.appId, callbacks -> callbacks.size() == 100000, vertx, testContext).onComplete(testContext.succeeding(callbacks -> {

          testContext.completeNow();

        }));
      }));
    }));

  }

}
