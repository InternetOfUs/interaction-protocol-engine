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

package eu.internetofus.wenet_interaction_protocol_engine.api.messages;

import static org.assertj.core.api.Assertions.assertThat;

import javax.ws.rs.core.Response.Status;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import eu.internetofus.common.components.incentive_server.BadgeTest;
import eu.internetofus.common.components.incentive_server.Incentive;
import eu.internetofus.common.components.interaction_protocol_engine.Message;
import eu.internetofus.wenet_interaction_protocol_engine.EngineWorker;
import io.vertx.core.Vertx;
import io.vertx.ext.web.api.OperationRequest;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

/**
 * Test the {@link MessagesResource}
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(VertxExtension.class)
public class MessagesResourceTest {

  /**
   * Check that when send the message over the event bus is captured.
   *
   * @param vertx       event bus to use.
   * @param testContext context of the running test.
   */
  @Test
  public void shouldSendMessageToEngineWorker(final Vertx vertx, final VertxTestContext testContext) {

    vertx.eventBus().consumer(EngineWorker.ADDRESSS, msg -> testContext.completeNow());
    final var resource = new MessagesResource(vertx);
    final var message = new Message();
    message.type = Message.Type.INCENTIVE;
    final var incentive = new Incentive();
    incentive.Badge = new BadgeTest().createModelExample(1);
    message.content = incentive.toJsonObject();
    final var body = message.toJsonObject();
    final var context = new OperationRequest();
    resource.sendMessage(body, context, testContext.succeeding(response -> testContext.verify(() -> {

      assertThat(response.getStatusCode()).isEqualTo(Status.ACCEPTED.getStatusCode());

    })));

  }

}
